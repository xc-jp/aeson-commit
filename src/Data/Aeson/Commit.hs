{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Data.Aeson.Commit
  ( commit
  , runCommit
  , Commit(..)
  , formatFail
  , failList
  , withKey
  , objWithKey
  , overArray
  , tryParser
  , liftParser
  , (.:>)
  ) where

import           Control.Applicative  (Alternative (..))
import           Control.Monad.Except
import           Data.Aeson.Types
import           Data.Foldable        (toList)
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text, unpack)
import           Data.Void            (Void, vacuous)

-- | A parser that has _two_ failure modes; the 'ExceptT' or in the underlying 'Parser'.
--   The alternative instance only recovers from failures in the `ExceptT`.
--   This means that, as soon as we successfully construct a 'Right' value, the 'Alternative' considers the 'Commit' a success, even though the inner 'Parser' can still fail.
--
--   The 'Void' guarantees that that parser contains an error value.
newtype Commit a = Commit {unCommit :: ExceptT [Parser Void] Parser a}
  deriving (Monad, Functor, Applicative, Alternative)

-- | Construct a commit.
--   If the first parser succeeds up until the running of the inner parser
--   the 'Commit' is a success and any failures in the inner parser will be preserved.
commit :: (forall r. (b -> Parser r) -> a -> Parser r) -> (b -> Parser c) -> a -> Commit c
commit super sub v
  = Commit $ ExceptT (captureError (super (const $ pure ()) v)) -- Lift super's error to the ExceptT level
  >> lift (super sub v) -- Keep sub's error in the outer (committed) Parser by
                        -- running again within super to preserve super's path
   where
     captureError :: Parser b -> Parser (Either [Parser Void] b)
     captureError p = Right <$> p <|> pure (Left [fmap (const undefined) p])

-- | Run a 'Commit' parser. If the 'Commit' parser fails to commit the error
-- messages in the resulting parser are formatted with the 'failList' function.
runCommit :: Commit a -> Parser a
runCommit (Commit (ExceptT e)) = e >>= either (formatFail failList) pure

-- | Convenience wrapper around 'commit' for when the commit is simply checking whether a key is present in some object.
--   If it is, it will append the key to the JSONPath of the inner context through '<?>'.
--   This is should give the proper JSON path for error messages
(.:>)  :: FromJSON a => Object -> Text -> (a -> Parser b) -> Commit b
(o .:> k) cont = commit (\p -> withKey k (parseJSON >=> p)) cont o

-- | Like '(.:)' but allows passing an explicit parser for the extracted 'Value'
withKey :: Text -> (Value -> Parser b) -> Object -> Parser b
withKey key p obj =
  case HM.lookup key obj of
    Nothing -> fail $ "key " <> show key' <> " not found"
    Just v  -> p v <?> Key key
  where key' = unpack key

-- | Equivalent to 'withObject key $ withKey key p'
objWithKey :: Text -> (Value -> Parser b) -> Value -> Parser b
objWithKey key p = withObject (unpack key) $ withKey key p

-- | Parses an array while including path index information
overArray :: (Value -> Parser a) -> Array -> Parser [a]
overArray p = traverse tag . flip zip [0..] . toList
  where
    tag (v,i) = p v <?> Index i

-- | Try to parse with a 'Parser' and commit if it parses successfully.
--   Unlike 'liftParser', the parser's failure is recoverable.
--
-- > tryParser p = commit p pure
tryParser :: Parser a -> Commit a
tryParser p = commit (\inner () -> p >>= inner) pure ()

-- | Turn a 'Parser' into a 'Commit'.
--   Unlike 'tryParser', the parser's failure is _not_ recoverable, i.e. the parse is always committed.
--
-- > liftParser p = commit (pure ()) (\() -> p) = Commit (lift p)
liftParser :: Parser a -> Commit a
liftParser p = commit ($) (const p) ()

-- | Format the error messages from the passed 'Parser's.
formatFail
  :: ([(JSONPath, String)] -> (JSONPath, String))
  -> [Parser Void]
  -> Parser a
formatFail f = handleErrors
  where
  handleErrors :: [Parser Void] -> Parser a
  handleErrors ps = vacuous (go ps [])
    where
    go [] errors =
      let (path, error) = f (reverse errors)
      in parserThrowError path error
    go (y:ys) msgs = parserCatchError y $ \path msg ->
      go ys ((path,msg):msgs)

-- | Output the many errors as a YAML encoded list
failList :: [(JSONPath, String)] -> (JSONPath, String)
failList [] = ([], "No parsers tried")
failList [(path, msg)] = (path, msg)
failList errors =
  ( [] -- We return '[]' here leaving the upstream parser to print the path
       -- to where this group of errors began
  , "No match,\n" <> unlines (map (uncurry showError) errors))
  where
  showError [] msg  = "- " <> msg
  showError rel msg = "- " <> formatPath rel <> ": " <> msg

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           Data.Char            (isAlpha, isAlphaNum)
import           Data.Foldable        (toList)
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text, pack, unpack)
import           Data.Void            (Void, vacuous)

-- | A parser that has _two_ failure modes; the 'ExceptT' or in the underlying 'Parser'.
--   The alternative instance only recovers from failures in the `ExceptT`.
--   This means that, as soon as we successfully construct a 'Right' value, the 'Alternative' considers the 'Commit' a success, even though the inner 'Parser' can still fail.
--
--   The 'Void' guarantees that that parser contains an error value.
newtype Commit a = Commit {unCommit :: ExceptT [Parser Void] Parser a}
  deriving (Monad, Functor, Applicative, Alternative)

-- | Construct a commit.
--   If the first parser succeeds, the 'Commit' is a success, and any failures in the inner action will be preserved.
commit :: Parser a -> (a -> Parser b) -> Commit b
commit pre post
  = Commit $ ExceptT (captureError pre) -- Lift pre's error to the ExceptT level
  >>= lift . post -- Keep post's error in the inner (committed) Parser
    where
      captureError :: Parser b -> Parser (Either [Parser Void] b)
      captureError p = Right <$> p <|> pure (Left [fmap (const undefined) p])

-- | Run a 'Commit' parser. If the 'Commit' parser fails to commit the error
-- messages in the resulting parser are formatted with the 'failList' function.
runCommit :: Commit a -> Parser a
runCommit (Commit (ExceptT e)) = e >>= either (formatFail failList) pure

-- | Convenience wrapper around 'commit' for when the commit is simply checking whether a key is present in some object.
--   If it is, it will append the key to the JSONPath of the inner context through '<?>'.
--   This is should give the proper JSON path for error messages, although I'm not entirely sure if this is idiomatic.
(.:>)  :: FromJSON a => Object -> Text -> (a -> Parser b) -> Commit b
(o .:> k) cont = commit (o .: k) (\v -> cont v <?> Key k)

-- | Like '(.:)' but allows passing an explicit parser for the extracted 'Value'
withKey :: String -> (Value -> Parser b) -> Object -> Parser b
withKey key' p obj =
  case HM.lookup key obj of
    Nothing -> fail $ "key " <> show key' <> " not found"
    Just v  -> p v <?> Key key
  where key = pack key'

-- | Equivalent to 'withObject key $ withKey key p'
objWithKey :: String -> (Value -> Parser b) -> Value -> Parser b
objWithKey key p = withObject key $ withKey key p

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
tryParser p = commit p pure

-- | Turn a 'Parser' into a 'Commit'.
--   Unlike 'tryParser', the parser's failure is _not_ recoverable, i.e. the parse is always committed.
--
-- > liftParser p = commit (pure ()) (\() -> p) = Commit (lift p)
liftParser :: Parser a -> Commit a
liftParser p = commit (pure ()) (const p)

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


-- | Format a <http://goessner.net/articles/JsonPath/ JSONPath> as a 'String'
-- which represents the path relative to some root object.
--
-- The function is exposed first in aeson-1.4.6.0 but we're building against aeson-1.4.5.0
formatRelativePath :: JSONPath -> String
formatRelativePath = format ""
  where
    format :: String -> JSONPath -> String
    format pfx []                = pfx
    format pfx (Index idx:parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key:parts)   = format (pfx ++ formatKey key) parts

    formatKey :: Text -> String
    formatKey key
       | isIdentifierKey strKey = "." ++ strKey
       | otherwise              = "['" ++ escapeKey strKey ++ "']"
      where strKey = unpack key

    isIdentifierKey :: String -> Bool
    isIdentifierKey []     = False
    isIdentifierKey (x:xs) = isAlpha x && all isAlphaNum xs

    escapeKey :: String -> String
    escapeKey = concatMap escapeChar

    escapeChar :: Char -> String
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c    = [c]

formatPath :: JSONPath -> String
formatPath path = "$" ++ formatRelativePath path

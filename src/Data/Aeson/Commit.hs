{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Commit
  ( commit
  , runCommit
  , Commit(..)
  , formatFail
  , failList
  , tryParser
  , liftParser
  , (.:>)
  ) where

import           Control.Applicative  (Alternative (..))
import           Control.Monad.Except
import           Data.Aeson.Types
import           Data.Text            (Text)
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
  :: ([(JSONPath, String)] -> String)
  -> [Parser Void]
  -> Parser a
formatFail f ps = vacuous (go ps [])
  where
  go [] errors =
    let error = f (reverse errors)
    in parserThrowError [] error
      -- The empty path ([]) is relative from the current parser's failure, not the absolute JSONPath
      -- Since `aeson` doesn't export any way of specifying an absolute JSONPath we cannot do better than
      -- this here.
  go (y:ys) msgs = parserCatchError y $ \path msg ->
    go ys ((path,msg):msgs)

-- | Output the many errors as a YAML encoded list. If the error
-- messages have different 'JSONPath's then the longest common prefix
-- is used in the top-level error message and the non-empty relative paths
-- from the top-level path is shown at the respective error message.
failList :: [(JSONPath, String)] -> String
failList [] = "No parsers tried"
failList [(_, msg)] = msg
failList errors =
  let (paths, msgs) = unzip errors
      common = commonPrefix paths
      relative = fmap (drop (length common)) paths
  in "No match,\n" <> unlines (zipWith showError relative msgs)
  where
  showError [] msg  = "- " <> msg
  showError rel msg = "- " <> formatRelativePath rel <> ": " <> msg
  commonPrefix :: Eq a => [[a]] -> [a]
  commonPrefix []     = []
  commonPrefix (x:xs) = foldr common x xs
    where
    common as bs = fst <$> takeWhile (uncurry (==)) (zip as bs)

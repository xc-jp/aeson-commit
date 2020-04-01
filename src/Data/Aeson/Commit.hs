{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Commit where

import           Control.Applicative  (Alternative (..))
import           Control.Monad.Except
import           Data.Aeson.Types
import           Data.Text            (Text)
import           Data.Void            (Void, absurd)

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
commit pre post = Commit $ do
  a <- ExceptT $ captureError pre -- Lift pre's error to the ExceptT level
  lift $ post a
    where
      captureError :: Parser b -> Parser (Either [Parser Void] b)
      captureError p = Right <$> p <|> pure (Left [fmap (const undefined) p])

runCommit :: Commit a -> Parser a
runCommit (Commit f) = runExceptT f >>= either handleErrors pure
  where
  handleErrors :: [Parser Void] -> Parser a
  handleErrors []     = fail "No parsers tried"
  handleErrors (p:ps) = fmap absurd (go (p:ps) [] [])
    where
    go [] path errors = parserThrowError path ("No match,\n" <> unlines (fmap ("- " <>) errors))
    go (y:ys) _ msgs = parserCatchError y $ \path msg ->
      go ys path (msg:msgs)
        -- TODO: how do we handle the multiple JSONPaths?
        -- Right now the rightmost failure's path is used when presenting
        -- the error message. Ideally one path per error would be preferable but
        -- `aeson` doesn't support such a thing. When errors are reported in `aeson`
        -- a single JSONPath defines how the error message is presented.

-- | Convenience wrapper around 'commit' for when the commit is simply checking whether a key is present in some object.
--   If it is, it will append the key to the JSONPath of the inner context through '<?>'.
--   This is should give the proper JSON path for error messages, although I'm not entirely sure if this is idiomatic.
(.:>)  :: FromJSON a => Object -> Text -> (a -> Parser b) -> Commit b
(o .:> k) cont = commit (o .: k) (\v -> cont v <?> Key k)

-- | Try to a 'Parser' and commit if it parses successfully.
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

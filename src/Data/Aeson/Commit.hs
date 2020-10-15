{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Commit where

import           Control.Applicative  (Alternative (..))
import           Control.Monad.Except
import           Data.Aeson.Types
import           Data.Text            (Text)
import           Data.Void            (Void, absurd)

-- | A 'Parser' that has _two_ failure modes; recoverable and non-recoverable.
--   The default, recoverable failure is the equivalent to aeson's default 'Parser' behavior.
--   The non-recoverable failure mode is used to commit to a branch; to commit means that every subsequent failure is non-recoverable.
--
--   The implementation works by capturing failure in either the 'ExceptT' or in the underlying 'Parser'.
--   The derived 'Alternative' instance will only recover from failures in the 'ExceptT'.
--   This means that as soon as we successfully construct a 'Right' value, the 'Alternative' considers the 'Commit' a success, even though the underlying parser might have failed.
--   The 'Void' represents the guarantee that we only collect error values.
newtype Commit a = Commit {unCommit :: ExceptT [Parser Void] Parser a}
  deriving (Monad, Functor, Applicative, Alternative)

-- | Construct a commit.
--   If the first parser succeeds, the 'Commit' is a success, and any failures in the inner action will be preserved.
commit :: Parser a -> (a -> Parser b) -> Commit b
commit pre post = Commit $ do
  a <- ExceptT $ captureError pre
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

-- | Convenience wrapper around 'commit' for when the commit is checking whether a key is present in some object.
--   If it is, it will commit and append the key to the JSONPath of the inner context through '<?>', which will give nicer error messages.
(.:>)  :: FromJSON a => Object -> Text -> (a -> Parser b) -> Commit b
(o .:> k) cont = commit (o .: k) (\v -> cont v <?> Key k)

-- | Try to parse with a 'Parser' and commit if it parses successfully.
--   Unlike 'liftParser', the parser's failure is recoverable.
--   
-- > tryParser empty <|> p = p
tryParser :: Parser a -> Commit a
tryParser p = commit p pure

-- | Turn a 'Parser' into a 'Commit'.
--   Unlike 'tryParser', the parser's failure is _not_ recoverable, i.e. the parse is always committed.
--   
-- > liftParser empty <|> p = empty
liftParser :: Parser a -> Commit a
liftParser p = commit (pure ()) (const p)

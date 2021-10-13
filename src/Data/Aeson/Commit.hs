{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--   Commit mechanism for aeson's 'Parser'.
--   To commit means that if some initial parsing succeeds, subsequent failures are unrecoverable.
--
--   In the following example, we use '.:>' to look for a key @.nested.value@, and if that does not exist, @.value@.
--
--   > parse o = (o .:> "nested") (withObject "nestedObj" (.: "value"))
--   >         <|> tryParser (o .: "value")
--
--   Not having the key @nested@ is a normal, recoverable failure, and parsing will continue looking for @value@.
--   However, if @nested@ is present but malformed, parsing fails.
--
--   > { value: "foo", otherField: "bar" }
--   > -> Right "foo"
--   >
--   > { value: "foo", nested: { value: "bar" } }
--   > -> Right "bar"
--   >
--   > { value: "foo", nested: { bar: 9 } }
--   > -> Left "Error in $.nested: key \"value\" not found"
--   >
--   > { value: "foo", nested: 9 }
--   > -> Left "Error in $.nested: parsing nestedObj failed, expected Object, but encountered Number"
--   >
--   > {}
--   > -> Left
--   >   "Error in $: No match,
--   >    - key \"value\" not found"
--   >    - key \"nested\" not found"
module Data.Aeson.Commit
  ( commit,
    runCommit,
    (.:>),
    tryParser,
    liftParser,
    Commit (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Except
import Data.Aeson.Types
import Data.Void (Void, absurd)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
#else
import Data.Text (Text)
#endif

-- | A 'Commit' is a 'Parser' that has two failure modes; recoverable and non-recoverable.
--
--   > tryParser empty <|> p = p
--   > liftParser empty <|> p = empty
--
--   'Commit' is typically constructed using 'commit', and consumed using 'runCommit', which captures its result in a 'Parser'.
--
--   The implementation works by wrapping 'Parser' in an 'ExceptT'.
--   The derived 'Alternative' instance will then only recover from failures in the 'ExceptT'.
--   This means that as soon as we successfully construct a 'Right' value, the 'Alternative' considers the 'Commit' a success, even though the underlying parser might have failed.
--   The 'Void' represents the guarantee that we only collect error values.
newtype Commit a = Commit {unCommit :: ExceptT [Parser Void] Parser a}
  deriving (Monad, Functor, Applicative, Alternative)

-- | Construct a commit.
--   If the first parser fails, the failure is recoverable through 'Alternative'.
--   If the first parser succeeds, the 'Commit' is a success, and any failures in the inner action will be preserved.
commit :: Parser a -> (a -> Parser b) -> Commit b
commit pre post = Commit $ do
  a <- ExceptT $ captureError pre
  lift $ post a
  where
    -- TODO maybe there's a better way to prove something is an error
    captureError :: Parser b -> Parser (Either [Parser Void] b)
    captureError p = Right <$> p <|> pure (Left [fmap (const undefined) p])

-- | Run a 'Commit', capturing its result in a 'Parser'.
runCommit :: Commit a -> Parser a
runCommit (Commit f) = runExceptT f >>= either handleErrors pure
  where
    handleErrors :: [Parser Void] -> Parser a
    handleErrors [] = fail "No parsers tried"
    handleErrors (p : ps) = fmap absurd (go (p : ps) [] [])
      where
        -- TODO: how do we handle the multiple JSONPaths?
        -- Right now the rightmost failure's path is used when presenting
        -- the error message. Ideally one path per error would be preferable but
        -- `aeson` doesn't support such a thing. When errors are reported in `aeson`
        -- a single JSONPath defines how the error message is presented.
        go [] path errors = parserThrowError path ("No match,\n" <> unlines (fmap ("- " <>) errors))
        go (y : ys) _ msgs = parserCatchError y $ \path msg ->
          go ys path (msg : msgs)

-- | Convenience wrapper around 'commit' for when the commit is checking whether a key is present in some object.
--   If it is, it will commit and append the key to the JSONPath of the inner context through '<?>', which will give nicer error messages.

#if MIN_VERSION_aeson(2,0,0)
(.:>)  :: FromJSON a => Object -> Key.Key -> (a -> Parser b) -> Commit b
#else
(.:>)  :: FromJSON a => Object -> Text -> (a -> Parser b) -> Commit b
#endif
(o .:> k) cont = commit (o .: k) (\v -> cont v <?> Key k)

-- | Turn a 'Parser' into a 'Commit'
--   Unlike 'liftParser', the parser's failure is recoverable.
--
-- > tryParser empty <|> p = p
-- > tryParser p = commit p pure
tryParser :: Parser a -> Commit a
tryParser p = commit p pure

-- | Turn a 'Parser' into a 'Commit'.
--   Unlike 'tryParser', the parser's failure is _not_ recoverable, i.e. the parse is always committed.
--
-- > liftParser empty <|> p = empty
-- > liftParser p = commit (pure ()) (const p)
liftParser :: Parser a -> Commit a
liftParser p = commit (pure ()) (const p)

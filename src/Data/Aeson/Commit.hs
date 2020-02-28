{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Data.Aeson.Commit where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import Data.Aeson.Types
import Data.Void (Void, absurd)
import Data.Monoid (Dual (..))
import Control.Monad.Except

-- | A parser that has _two_ failure modes; the 'ExceptT' or in the underlying 'Parser'.
--   The alternative instance only recovers from failures in the `ExceptT`.
--   This means that, as soon as we successfully construct a 'Right' value, the 'Alternative' considers the 'Commit' a success, even though the inner 'Parser' can still fail.
--
--   The 'Void' guarantuees that that parser contains an error value.
newtype Commit a = Commit {unCommit :: ExceptT (Parser Void) Parser a}
  deriving (Monad, Functor, Applicative)
  deriving Alternative via (ExceptT (Dual (Parser Void)) Parser)
    -- If both e1 and e2 fail, the default Alternative instance would mean (e1 <|> e2) = e2. This makes it so that it fails as e1.
    -- To elaborate: The Alternative instance for ExceptT combines errors using <>. The <> for Parser is <|>, which is right-leaning.
    -- By going through the 'Dual', we make it left-leaning, which seems to be the better behavior, but I'm interested in hearing arguments to the contrary.

-- | Construct a commit.
--   If the first parser succeeds, the 'Commit' is a success, and any failures in the inner action will be preserved.
commit :: Parser a -> (a -> Parser b) -> Commit b
commit pre post = Commit $ do
  a <- ExceptT $ captureError pre -- Lift pre's error to the ExceptT level
  lift $ post a
    where
      captureError :: Parser b -> Parser (Either (Parser Void) b)
      captureError p = Right <$> p <|> pure (Left $ undefined <$> p)

runCommit :: Commit a -> Parser a
runCommit (Commit f) = runExceptT f >>= either (fmap absurd) pure

-- | Convenience wrapper around 'commit' for when the commit is simply checking whether a key is present in some object.
--   If it is, it will append the key to the JSONPath of the inner context through '<?>'.
--   This is should give the proper JSON path for error messages, although I'm not entirely sure if this is idiomatic.
(.:>)  :: FromJSON a => Object -> T.Text -> (a -> Parser b) -> Commit b
(o .:> k) cont = commit (o .: k) (\v -> cont v <?> Key k)

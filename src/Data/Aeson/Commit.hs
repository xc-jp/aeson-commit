{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Data.Aeson.Commit
    ( commit
    , runCommit
    , Commit
    , parseKey
    , matchKey
    , parseCommit
    , decodeJSONFile
    , decodeYamlFile
    )
where

import           Control.Applicative
import           Control.Monad              (join, (>=>))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types           hiding (parse)
import           Data.Bifunctor             (first, second)
import           Data.Text                  (Text, unpack)
import           Data.Yaml                  (decodeFileEither,
                                             prettyPrintParseException)

-- | A commit parser
newtype Commit x = Commit { unCommit :: ReaderT Value (MaybeT Parser) x}
  deriving (Functor, Applicative, Monad)

instance Alternative Commit where
  empty = Commit . ReaderT . const . MaybeT . pure $ Nothing
  a <|> b = Commit $ ReaderT $ \v -> MaybeT $ do
    x <- runMaybeT $ runReaderT (unCommit a) v
    case x of
      Nothing -> runMaybeT $ runReaderT (unCommit b) v
      Just y  -> pure (Just y)

-- | Create a commit parser that doesn't backtrack if the first parser parses
-- successfully.
commit :: (Value -> Parser x) -> (x -> Parser y) -> Commit y
commit f g = Commit . ReaderT $ MaybeT . pure . parseMaybe f >=> lift . g

-- | Run a commit parser by picking the first matching parser that commits.
-- The returned parser fails if no parser matches.
runCommit :: Commit x -> Value -> Parser x
runCommit go value = runMaybeT (runReaderT (unCommit go) value) >>= matched
  where
   matched Nothing  = fail $ "No parser matches value " <> show value
   matched (Just y) = pure y

-- | Parse a key by name in an object.
parseKey
  :: FromJSON a
  => Text
  -> Value
  -> Parser a
parseKey key = withObject (unpack key) $ \o -> o .: key

-- | Match key against an object or a string.
-- If the parsed Value is an object ensure that the object has the key.
-- If the parsed Value is a string match the key against the string.
matchKey
  :: Text
  -> Value
  -> Parser ()
matchKey key v = withObject (unpack key) (.: key) v
  <|> withText (unpack key) (\txt ->
      if key == txt
        then pure ()
        else fail $ "key mismatch got " <> unpack txt <> ", expected " <> unpack key
      ) v

-- | Run a commit parser on a Value
parseCommit :: Commit t -> Value -> Either String t
parseCommit parser = parseEither (runCommit parser)

-- | Decode a file with a commit parser given a way to decode the file into a Value.
decodeFileWith
  :: (FilePath -> IO (Either String Value))
  -> Commit a -> FilePath -> IO (Either String a)
decodeFileWith decoder c = fmap (join . second (parseCommit c)) . decoder

-- | Decode a JSON-encoded file.
decodeJSONFile :: Commit a -> FilePath -> IO (Either String a)
decodeJSONFile = decodeFileWith eitherDecodeFileStrict

-- | Decode a YAML-encoded file.
decodeYamlFile :: Commit a -> FilePath -> IO (Either String a)
decodeYamlFile = decodeFileWith (fmap (first prettyPrintParseException) . decodeFileEither)

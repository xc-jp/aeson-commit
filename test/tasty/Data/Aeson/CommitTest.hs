{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.CommitTest (tests) where

import           Control.Applicative
import           Data.Aeson.Commit
import           Data.Aeson.Types
import           Data.Text           (unpack)
import           Test.Tasty.Hspec

tests :: Spec
tests =
  describe "Backtracking" $ do
    it "commits" commitTest
    it "backtracks" backtrackTest
    it "no match" noMatchTest
    it "nested fail" nestedFail

commitTest :: IO ()
commitTest
  = parseCommit parser value
  `shouldBe`
  Left "Error in $: parsing hello failed, expected Object, but encountered String"
  where
    value = object ["a" .= ("c" :: String)]
    parser :: Commit ()
    parser =
      commit (parseKey "a") (withObject "hello" (const $ pure ()))
      <|> commit (parseKey "a") (\() -> pure ())

backtrackTest :: IO ()
backtrackTest
  = parseCommit parser value
  `shouldBe` Right "c"
  where
  value = object ["a" .= ("c" :: String)]
  parser :: Commit String
  parser =
    commit (parseKey "b") (\() -> fail "no!")
    <|> commit (parseKey "a") (withText "testing" (pure . unpack))

noMatchTest :: IO ()
noMatchTest
  = parseCommit parser value
  `shouldBe` Left "Error in $: No parser matches value Object (fromList [(\"a\",String \"c\")])"
  where
  value = object ["a" .= ("c" :: String)]
  parser :: Commit String
  parser =
    commit (parseKey "b") (\() -> fail "no!")
    <|> commit (parseKey "c") (withText "testing" (pure . unpack))

fromValue :: Value -> a -> a
fromValue _ = id

nestedFail :: IO ()
nestedFail = parseCommit parser value `shouldBe` Left "Error in $: blaargh" -- The path should actually be Error in $.a.c: ...
  where
  value = object ["a" .= object ["c" .= ("d" :: String)]]
  parser :: Commit String
  parser = commit (parseKey "a") (runCommit inner)
  inner :: Commit String
  inner = commit (parseKey "c") (\v -> fromValue v (fail "blaargh"))

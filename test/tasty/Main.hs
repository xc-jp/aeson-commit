{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative
import Data.Aeson.Commit
import Data.Aeson.QQ
import Data.Aeson.Types
import Data.Text (Text)
import Test.Hspec

tests :: Spec
tests =
  testParserWithCases
    pNested
    [ ( "fails",
        [aesonQQ| {} |],
        Left $
          unlines
            [ "Error in $: No match,",
              "- key \"value\" not found",
              "- key \"nested\" not found"
            ]
      ),
      ( "succeeds unnested",
        [aesonQQ| { value: "top" } |],
        Right "top"
      ),
      ( "succeeds and prefers nested",
        [aesonQQ| { value: "top" , nested: { value: "nest" } } |],
        Right "nest"
      ),
      ( "fails on malformed nested",
        [aesonQQ| { value: "top", nested: { foo: 9 } } |],
        Left "Error in $.nested: key \"value\" not found"
      ),
      ( "fails on nested type mismatch",
        [aesonQQ| { value: "top", nested: 9 } |],
        Left "Error in $.nested: parsing nestedObj failed, expected Object, but encountered Number"
      )
    ]
  where
    pNested :: Value -> Parser Text
    pNested = withObject "topLevel" $ \o ->
      runCommit $
        (o .:> "nested") (withObject "nestedObj" (.: "value"))
          <|> tryParser (o .: "value")

testParserWithCases ::
  (Eq a, Show a) =>
  (v -> Parser a) ->
  [(String, v, Either String a)] ->
  Spec
testParserWithCases p =
  mapM_ (\(name, v, result) -> it name (parseEither p v `shouldBe` result))

main :: IO ()
main = hspec tests

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Aeson.CommitTest (tests) where

import           Control.Applicative
import           Data.Aeson.Commit
import           Data.Aeson.QQ
import           Data.Aeson.Types
import           Data.Text           (Text)
import           Test.Tasty.Hspec

tests :: Spec
tests = do
  testParserWithCases pNested
    [ ( "fails"
      , [aesonQQ| {} |]
      , Left $ unlines
        [ "Error in $: No match,"
        , "- key \"nested\" not found"
        , "- key \"value\" not found"
        ]
      )
    , ( "succeeds unnested"
      , [aesonQQ| { value: "top" } |]
      , Right "top"
      )
    , ( "succeeds and prefers nested"
      , [aesonQQ| { value: "top" , nested: { value: "nest" } } |]
      , Right "nest"
      )
    , ( "fails on malformed nested"
      , [aesonQQ| { value: "top", nested: { foo: 9 } } |]
      , Left "Error in $.nested: key \"value\" not found"
      )
    , ( "fails on nested type mismatch"
      , [aesonQQ| { value: "top", nested: 9 } |]
      , Left "Error in $.nested: parsing nestedObj failed, expected Object, but encountered Number"
      )
    ]
  testParserWithCases parser2
    [ ("fails with relative path"
      , [aesonQQ| {"foo": {}} |]
      , Left $ unlines
        [ "Error in $: No match,"
        , "- parsing array failed, expected Array, but encountered Object"
        , "- $.foo: key \"bar\" not found"
        ]
      )
    , ("fails with nested relative path"
      , [aesonQQ| {"foo": {"bar": ["hello"]}} |]
      , Left $ unlines
        [ "Error in $: No match,"
        , "- parsing array failed, expected Array, but encountered Object"
        , "- $.foo.bar[0]: parsing Int failed, expected Number, but encountered String"
        ]
      )
    , ("fails after commitment"
      , [aesonQQ| [{}] |]
      , Left "Error in $[0]: parsing Int failed, expected Number, but encountered Object"
      )
    ]
  testParserWithCases
    (withArray "arr" (overArray parser2))
    [ ("path remains correct for a commit nested within another parser"
      , [aesonQQ| [ [1, 2, 3], {"foo": {"bar": ["hello"]}} ] |]
      , Left $ unlines
        [ "Error in $[1]: No match,"
        , "- $[1]: parsing array failed, expected Array, but encountered Object"
        , "- $[1].foo.bar[0]: parsing Int failed, expected Number, but encountered String"
        ]
      )
    ]
  testParserWithCases
    (runCommit . commit (objWithKey "foo") (parseJSON :: Value -> Parser Int))
    [ ("subparser retains path from super-parser"
      , [aesonQQ| { "foo": "fail" } |]
      , Left "Error in $.foo: parsing Int failed, expected Number, but encountered String"
      )
    ]
  where
    parser2 :: Value -> Parser [Int]
    parser2 v = runCommit $
      commit (withArray "array") (overArray parseJSON) v
      <|> tryParser (objWithKey "foo" (objWithKey "bar" parseJSON) v)
    pNested :: Value -> Parser Text
    pNested = withObject "topLevel" $ \o ->
      runCommit
        $ (o .:> "nested") (withObject "nestedObj" (.: "value"))
        <|> tryParser (o .: "value")


testParserWithCases
  :: (Eq a, Show a)
  => (v -> Parser a)
  -> [(String, v, Either String a)]
  -> Spec
testParserWithCases p =
  mapM_ ( \(name, v, result) -> it name (parseEither p v `shouldBe` result))

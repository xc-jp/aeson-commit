{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Aeson.CommitTest (tests) where

import           Control.Applicative
import           Control.Monad       (zipWithM)
import           Data.Aeson.Commit
import           Data.Aeson.QQ
import           Data.Aeson.Types
import           Data.Foldable       (toList)
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
        , "- .foo: key \"bar\" not found"
        ]
      )
    , ("fails with nested relative path"
      , [aesonQQ| {"foo": {"bar": ["hello"]}} |]
      , Left $ unlines
        [ "Error in $: No match,"
        , "- parsing array failed, expected Array, but encountered Object"
        , "- .foo.bar[0]: parsing Int failed, expected Number, but encountered String"
        ]
      )
    , ("fails after commitment"
      , [aesonQQ| [{}] |]
      , Left "Error in $: parsing Int failed, expected Number, but encountered Object"
      )
    ]
  testParserWithCases (\v -> runCommit $ commit (withArray "arr" pure v) (parseJSONListWith parser2))
    [ ("includes common path"
      , [aesonQQ| [ [1, 2, 3], {"foo": {"bar": ["hello"]}} ] |]
      , Left $ unlines
        [ "Error in $[1]: No match,"
        , "- parsing array failed, expected Array, but encountered Object"
        , "- .foo.bar[0]: parsing Int failed, expected Number, but encountered String"
        ]
      )
    ]
  testParserWithCases (\v -> runCommit
    $   (commit (withObject "o" (withKey "foo" (.: "bar")) v) pure :: Commit Value)
    <|> (commit (withObject "o" (withKey "foo" (.: "baz")) v) pure :: Commit Value)
    )
    [ ("includes common path"
      , [aesonQQ| {"foo": {"bax": ["hello"]}} |]
      , Left $ unlines
        [ "Error in $.foo: No match,"
        , "- key \"bar\" not found"
        , "- key \"baz\" not found"
        ]
      )
    ]
  where
    parseJSONListWith :: (Value -> Parser a) -> Array -> Parser [a]
    parseJSONListWith f =
          zipWithM (\i v -> f v <?> Index i) ([0..] :: [Int])
        . toList
    withKey :: FromJSON a => Text -> (a -> Parser b) -> Object -> Parser b
    withKey key p o = o .: key >>= \v -> p v <?> Key key
    parser2 :: Value -> Parser [Int]
    parser2 v= runCommit $
      commit (withArray "array" pure v) (fmap toList . traverse parseJSON)
      <|> commit (withObject "object" (withKey "foo" (withKey "bar" parseJSON)) v) pure
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

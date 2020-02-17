module Main (main) where

import qualified Data.Aeson.CommitTest
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    t <- testGroup "Commit" <$> tests
    defaultMain t

tests :: IO [TestTree]
tests = do
   tests <- traverse testSpecs
    [ Data.Aeson.CommitTest.tests
    ]
   pure (concat tests)

module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Something" $ do
    it "blah" $ do
      1 + 1 `shouldBe` 2

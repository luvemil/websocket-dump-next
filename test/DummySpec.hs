module DummySpec where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
{-# NOINLINE spec #-}
spec =
    describe "Dummy test" $ do
        it "item should be equal to itself" $
            (3 :: Int) `shouldBe` 3

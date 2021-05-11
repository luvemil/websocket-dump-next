module Domain.CryptoWatchSpec where

import Domain.CryptoWatch.WS
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
{-# NOINLINE spec #-}
spec = describe "CryptoWatch WS" $ do
    it "build the correct payload" $
        krakenSub `shouldBe` "{\"subscribe\":{\"subscriptions\":[{\"streamSubscription\":{\"resource\":\"markets:87:trades\"}}]}}"
  where
    krakenSub = subscribePayload ["markets:87:trades"]
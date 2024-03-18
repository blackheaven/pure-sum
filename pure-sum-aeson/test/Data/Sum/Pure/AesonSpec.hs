{-# LANGUAGE StandaloneDeriving #-}

module Data.Sum.Pure.AesonSpec
  ( main,
    spec,
  )
where

import Data.Aeson
import qualified Data.Map as Map
import Data.Sum.Pure.Aeson
import GHC.Generics
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "encoding regular derivation is broken" $
    encode (Map.singleton RSunday ("Happy day" :: String)) `shouldBe` "[[\"RSunday\",\"Happy day\"]]"
  it "encoding pure sum derivation is working" $
    encode (Map.singleton BSunday ("Happy day" :: String)) `shouldBe` "{\"BSunday\":\"Happy day\"}"
  it "decoding pure sum derivation is working" $
    eitherDecode "{\"BSunday\":\"Happy day\"}" `shouldBe` Right (Map.singleton BSunday ("Happy day" :: String))
  it "decoding unknown pure sum derivation is working" $
    eitherDecode @(Map.Map BetterWeekend String) "{\"Sunday\":\"Happy day\"}" `shouldBe` Left "Error in $.Sunday: unknown value: \"Sunday\""

data RegularWeekend = RSaturday | RSunday
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data BetterWeekend = BSaturday | BSunday
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToSumText, FromSumText, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via (PureSum BetterWeekend)

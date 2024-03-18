{-# LANGUAGE StandaloneDeriving #-}

module Data.Sum.PureSpec
  ( main,
    spec,
  )
where

import Data.Proxy
import Data.Sum.Pure
import GHC.Generics
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "toSumText on unchanged derivation should be literal" $
    toSumText A1 `shouldBe` "A1"
  it "fromSumText on unchanged derivation should work" $
    fromSumText "A1" `shouldBe` Just A1
  it "toSumText on changed derivation should be changed (composition order matter)" $
    toSumText PfAlphaThing2 `shouldBe` "alpha-thing2"
  it "toSumText on changed derivation should be changed (composition order does not matter)" $
    toSumText BetaThing2 `shouldBe` "beta-thing2"
  it "fromSumText on changed derivation should work (composition order matter)" $
    fromSumText "alpha-thing2" `shouldBe` Just PfAlphaThing2
  it "fromSumText on second element of a changed derivation should work (composition order does matter)" $
    fromSumText "beta-thing2" `shouldBe` Just BetaThing2
  it "fromSumText not bamthing should be @Nothing@" $
    fromSumText @X2 "unknown" `shouldBe` Nothing

-- *  sandbox

--
-- data X0
--   deriving stock (Generic)
--
-- deriving via (PureSum X0) instance ToSumText X0
--
-- deriving via (PureSum X0) instance FromSumText X0

data X1 = A1
  deriving stock (Eq, Show, Generic)

deriving via (PureSum X1) instance ToSumText X1

deriving via (PureSum X1) instance FromSumText X1

data X2 = PfAlphaThing2 | BetaThing2
  deriving stock (Eq, Show, Generic)
  deriving (ToSumText, FromSumText) via (PureSumWith (DropPrefix "pf-" <<< SpinalCase) X2)

-- data X3 = A3 () | B3
--   deriving stock (Generic)
--
-- deriving via (PureSum X3) instance ToSumText X3
--
-- deriving via (PureSum X3) instance FromSumText X3

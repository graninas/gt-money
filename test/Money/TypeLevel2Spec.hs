{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}

module Money.TypeLevel2Spec where

import qualified Money.Raw.Money as RM
import qualified Money.TypeSafe1.Money as TS

import           Test.Hspec

import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol)



spec :: Spec
spec =
  describe "Type level money" $ do
    it "dummy" $ 1 `shouldBe` 1

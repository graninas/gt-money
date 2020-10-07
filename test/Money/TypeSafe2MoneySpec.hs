module Money.TypeSafe2MoneySpec where

import qualified Money.Raw.Money as RM
import qualified Money.TypeSafe2.Money as TS

import           Test.Hspec

getTotalAmountBroken
  :: TS.Currency cur1
  => TS.Currency cur2
  => TS.TSMoney cur1
  -> TS.TSMoney cur2
  -> TS.TSMoney cur2
getTotalAmountBroken tsm1 tsm2 =
  TS.add (TS.conv 10 tsm1) tsm2      -- raw add can't be used here

getTotalAmountInterpretable
  :: TS.Currency cur1
  => TS.Currency cur2
  => TS.TSMoney cur1
  -> TS.TSMoney cur2
  -> TS.TSMoney cur2
getTotalAmountInterpretable tsm1 tsm2 =
  TS.add' (TS.conv 10 tsm1) tsm2

instance TS.Currency "EUR"
instance TS.Currency "USD"

spec :: Spec
spec =
  describe "Type safe money, approach 2 (GADTs)" $ do
    it "Creation" $ do
      let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 4
      let tsm2 = TS.mkTSMoney @"USD" $ RM.mkMoney 4

      case (tsm1, tsm2) of
        (TS.MkTSMoney m1, TS.MkTSMoney m2) -> m1 `shouldBe` m2
        _ -> error "Test failed."

    it "Addition" $ do
      let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 4
      let tsm2 = TS.mkTSMoney @"EUR" $ RM.mkMoney 5
      let tsm3 = TS.add tsm1 tsm2

      case tsm3 of
        TS.MkTSMoney m -> m `shouldBe` RM.Money 9
        _ -> error "Test failed."

-- Won't compile
    -- it "Addition types mismatch" $ do
    --   let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 4
    --   let tsm2 = TS.mkTSMoney @"USD" $ RM.mkMoney 4
    --   let tsm3 = TS.add tsm1 tsm2
    --   case tsm3 of
    --     TS.MkTSMoney m -> m `shouldBe` RM.Money 8
    --     _ -> error "Test failed."

    describe "Conversion tests" $ do

      let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 10
      let tsm2 = TS.mkTSMoney @"USD" $ RM.mkMoney 5

      it "Total EUR -> USD" $ do
        let total = getTotalAmountInterpretable tsm1 tsm2
        let m = TS.runTSMoney total
        m `shouldBe` RM.Money 105

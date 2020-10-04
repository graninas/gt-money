module Money.TypeSafe1MoneySpec where

import qualified Money.Raw.Money as RM
import qualified Money.TypeSafe1.Money as TS

import           Test.Hspec


data Converter cur1 cur2 where
  Converter
   :: (TS.TSMoney cur1 -> TS.TSMoney cur2)
   -> Converter cur1 cur2

-- * Stacking not possible with this design.
getTotalAmount
  :: TS.Currency cur1
  => TS.Currency cur2
  => Converter cur1 cur2
  -> TS.TSMoney cur1
  -> TS.TSMoney cur2
  -> IO (TS.TSMoney cur2)
getTotalAmount (Converter f) tsm1 tsm2 = do
  let tsm1' = f tsm1
  pure $ TS.add tsm1' tsm2

instance TS.Currency "EUR"
instance TS.Currency "USD"

spec :: Spec
spec =
  describe "Money" $ do
    it "Simple operations" $ do
      let m1 = RM.Money 4
      let m2 = RM.Money 6
      let m3 = RM.add m1 m2
      m3 `shouldBe` RM.Money 10

    let convEurUsd = Converter @"EUR" @"USD" $ \m -> TS.convWithRate m 10
    let convUsdEur = Converter @"USD" @"EUR" $ \m -> TS.convWithRate m 2

    it "Total EUR -> USD" $ do
      total <- getTotalAmount convEurUsd (TS.mkTSMoney @"EUR" $ RM.mkMoney 10)  (TS.mkTSMoney @"USD" $ RM.mkMoney 5)
      total `shouldBe` (TS.mkTSMoney @"USD" $ RM.mkMoney 105)

-- wont compile, conv is Converter "EUR" "USD", not vice versa
    -- it "Total USD -> EUR" $ do
    --   total <- getTotalAmount convEurUsd (TS.mkTSMoney @"USD" $ RM.mkMoney 10)  (TS.mkTSMoney @"EUR" $ RM.mkMoney 5)
    --   total `shouldBe` (TS.mkTSMoney @"EUR" $ RM.mkMoney 105)

    it "Total USD -> EUR" $ do
      total <- getTotalAmount convUsdEur (TS.mkTSMoney @"USD" $ RM.mkMoney 10)  (TS.mkTSMoney @"EUR" $ RM.mkMoney 5)
      total `shouldBe` (TS.mkTSMoney @"EUR" $ RM.mkMoney 25)

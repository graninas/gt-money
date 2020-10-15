module Money.TypeSafe3MoneySpec where

import qualified Money.Raw.Money as RM
import qualified Money.TypeSafe3.Money as TS

import           Test.Hspec



-- Per-currency instances.
-- Very verbose.

-- Note: instead of type level strings you may use another type,
--   including an empty ADT:
--   data USD

instance TS.TSCurrency "USD" where
  data TSMoney "USD" = MkMoneyUSD RM.Money
  zero = MkMoneyUSD RM.zero
  mkTSMoney = MkMoneyUSD
  getMoney (MkMoneyUSD m) = m
  add (MkMoneyUSD m1) (MkMoneyUSD m2) = MkMoneyUSD $ RM.add m1 m2
  mul (MkMoneyUSD m1) v = MkMoneyUSD $ RM.mul m1 v

instance TS.TSCurrency "EUR" where
  data TSMoney "EUR" = MkMoneyEUR RM.Money
  zero = MkMoneyEUR RM.zero
  mkTSMoney = MkMoneyEUR
  getMoney (MkMoneyEUR m) = m
  add (MkMoneyEUR m1) (MkMoneyEUR m2) = MkMoneyEUR $ RM.add m1 m2
  mul (MkMoneyEUR m1) v = MkMoneyEUR $ RM.mul m1 v


-- Doesn't support multiple currencies at once:
-- data CryptoCoin
--
-- instance TS.TSCurrency CryptoCoin where
--   data TSMoney CryptoCoin
--     = BTC RM.Money
--     | ETH RM.Money
--
--   -- What coin should be here??
--   zero = ??? RM.zero
--   mkTSMoney = ???
--   add (??? m1) (??? m2) = ??? $ RM.add m1 m2
--   mul (??? m1) v = ??? $ RM.mul m1 v

getTotalAmount
  :: (TS.TSCurrency cur1, TS.TSCurrency cur2)
  => TS.TSMoney cur1
  -> TS.TSMoney cur2
  -> TS.TSMoney cur2
getTotalAmount tsm1 tsm2 =
  TS.add (TS.conv 10 tsm1) tsm2


spec :: Spec
spec =
  describe "Type safe money, approach 3 (Type Families)" $ do
    it "Creation" $ do
      let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 4
      let tsm2 = TS.mkTSMoney @"USD" $ RM.mkMoney 4

      case (tsm1, tsm2) of
        (MkMoneyEUR m1, MkMoneyUSD m2) -> m1 `shouldBe` m2

    it "Addition" $ do
      let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 4
      let tsm2 = TS.mkTSMoney @"EUR" $ RM.mkMoney 5
      let tsm3 = TS.add tsm1 tsm2

      case tsm3 of
        MkMoneyEUR m -> m `shouldBe` RM.Money 9

-- -- Won't compile
--     it "Addition types mismatch" $ do
--       let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 4
--       let tsm2 = TS.mkTSMoney @"USD" $ RM.mkMoney 4
--       let tsm3 = TS.add tsm1 tsm2
--       case tsm3 of
--         MkMoneyEUR m -> m `shouldBe` RM.Money 8

    describe "Conversion tests" $ do

      let tsm1 = TS.mkTSMoney @"EUR" $ RM.mkMoney 10
      let tsm2 = TS.mkTSMoney @"USD" $ RM.mkMoney 5

      it "Total EUR -> USD" $ do
        let total = getTotalAmount tsm1 tsm2
        case total of
          MkMoneyUSD m -> m `shouldBe` RM.Money 105

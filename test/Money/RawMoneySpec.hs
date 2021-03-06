module Money.RawMoneySpec where

import Money.Raw.Money

import           Test.Hspec


getTotalAmountUntypedUnsafeMoney :: [Money] -> IO Money
getTotalAmountUntypedUnsafeMoney ms = pure $ foldr add zero ms

-- Not safe. Solutions:
-- NonEmpty
-- Explicit currency to return when []
-- Not type safe.
getTotalAmountUntypedUnsafeCurrency :: [CMoney] -> IO CMoney
getTotalAmountUntypedUnsafeCurrency [] = error "empty list"
getTotalAmountUntypedUnsafeCurrency (cm : cms) =
  pure $ foldr add' cm cms
  where
    add' (CMoney cur1 m1) (CMoney cur2 m2)
      | cur1 == cur2 = CMoney cur1 $ add m1 m2
      | otherwise = error "Currencies are different."



spec :: Spec
spec =
  describe "Money" $ do
    it "Simple operations" $ do
      let m1 = Money 4
      let m2 = Money 6
      let m3 = add m1 m2
      m3 `shouldBe` Money 10
    it "Untyped unsafe money total" $ do
      total <- getTotalAmountUntypedUnsafeMoney [Money 10, Money 5]
      total `shouldBe` Money 15
    it "Untyped unsafe currency total" $ do
      total <- getTotalAmountUntypedUnsafeCurrency [CMoney EUR $ Money 10, CMoney EUR $ Money 5]
      total `shouldBe` CMoney EUR (Money 15)

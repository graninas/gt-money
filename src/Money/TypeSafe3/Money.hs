module Money.TypeSafe3.Money where

import qualified Money.Raw.Money as RM


-- Opt 3, Associated types and type families.
--  This design is open. You can add your own currencies when you want.

-- Note: I'm constantly forgetting the syntax of Type Families.

class TSCurrency cur where
  data TSMoney cur :: *
  zero :: TSMoney cur
  mkTSMoney :: RM.Money -> TSMoney cur
  getMoney :: TSMoney cur -> RM.Money
  add :: TSMoney cur -> TSMoney cur -> TSMoney cur
  mul :: TSMoney cur -> Int -> TSMoney cur


-- Currency-agnostic converter.

conv
  :: forall cur1 cur2
   . TSCurrency cur1
  => TSCurrency cur2
  => Int
  -> TSMoney cur1
  -> TSMoney cur2
conv r tsm = mkTSMoney $ RM.mul (getMoney tsm) r


--
--
-- mkTSMoney :: Currency cur => RM.Money -> TSMoney cur
-- mkTSMoney = MkTSMoney
--
-- zero :: Currency cur => TSMoney cur
-- zero = mkTSMoney RM.zero
--
-- -- Raw add (doesn't support conv)
-- add :: Currency cur => TSMoney cur -> TSMoney cur -> TSMoney cur
-- add (MkTSMoney m1) (MkTSMoney m2) = MkTSMoney $ RM.add m1 m2
-- add _ _ = error "Not implemented"
--
-- -- Interpretable add
-- add' :: Currency cur => TSMoney cur -> TSMoney cur -> TSMoney cur
-- add' m1 m2 = Add' (m1, m2)
--
-- -- Interpretable conv
-- conv :: (Currency cur1, Currency cur2) => Int -> TSMoney cur1 -> TSMoney cur2
-- conv v tsm = Conv v tsm


-- Basic interpreter (draft design)
-- This can also be
-- runTSMoney :: TSMoney cur -> TSMoney cur

-- runTSMoney :: TSMoney cur -> RM.Money
-- runTSMoney (MkTSMoney m) = m
-- runTSMoney (Add' (tsm1, tsm2)) = let
--   m1 = runTSMoney tsm1
--   m2 = runTSMoney tsm2
--   in RM.add m1 m2
-- runTSMoney (Conv v tsm) = let
--   m = runTSMoney tsm
--   in RM.mul m v

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

module Money.TypeSafe1.Money where

import qualified Money.Raw.Money as RM



-- Questions:
-- Precision
-- Parsing
-- Overflow and underflow
-- Currency
-- UX

-- Opt 1, open design

data TSMoney cur = TSMoney RM.Money
  deriving (Show, Read, Eq, Ord)

class Currency cur


mkTSMoney :: Currency cur => RM.Money -> TSMoney cur
mkTSMoney = TSMoney

zero :: Currency cur => TSMoney cur
zero = mkTSMoney RM.zero


add :: Currency cur => TSMoney cur -> TSMoney cur -> TSMoney cur
add (TSMoney m1) (TSMoney m2) = TSMoney $ RM.add m1 m2

convWithRate
  :: forall cur1 cur2
   . Currency cur1
  => Currency cur2
  => TSMoney cur1
  -> Int
  -> TSMoney cur2
convWithRate (TSMoney m1) v = mkTSMoney $ RM.mul m1 v

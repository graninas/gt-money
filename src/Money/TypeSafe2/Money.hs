module Money.TypeSafe2.Money where

import qualified Money.Raw.Money as RM


-- Opt 2, GADTs
-- currency type variable is not limited, but the constructors have constraints.
-- It's possible to declare a type synonym
--    type TSMBool = TSMoney Bool
-- But impossible to create a value of this type (if there is no instance of Currency for Bool)

class Currency cur

data TSMoney currency where

  MkTSMoney
    :: forall cur
     . Currency cur
    => RM.Money
    -> TSMoney cur

-- Interpretable add, can be configured (precision, rounding, overflow etc)
  Add'
    :: forall cur
     . Currency cur
    => (TSMoney cur, TSMoney cur)
    -> TSMoney cur

  Conv
    :: forall cur1 cur2
     . (Currency cur1, Currency cur2)
    => Int            -- Predefined rate
    -> TSMoney cur1
    -> TSMoney cur2

  -- deriving (Show, Read, Eq, Ord)



mkTSMoney :: Currency cur => RM.Money -> TSMoney cur
mkTSMoney = MkTSMoney

zero :: Currency cur => TSMoney cur
zero = mkTSMoney RM.zero

-- Raw add (doesn't support conv)
add :: Currency cur => TSMoney cur -> TSMoney cur -> TSMoney cur
add (MkTSMoney m1) (MkTSMoney m2) = MkTSMoney $ RM.add m1 m2
add _ _ = error "Not implemented"

-- Interpretable add
add' :: Currency cur => TSMoney cur -> TSMoney cur -> TSMoney cur
add' m1 m2 = Add' (m1, m2)

-- Interpretable conv
conv :: (Currency cur1, Currency cur2) => Int -> TSMoney cur1 -> TSMoney cur2
conv v tsm = Conv v tsm


-- Basic interpreter (draft design)
-- This can also be
-- runTSMoney :: TSMoney cur -> TSMoney cur

runTSMoney :: TSMoney cur -> RM.Money
runTSMoney (MkTSMoney m) = m
runTSMoney (Add' (tsm1, tsm2)) = let
  m1 = runTSMoney tsm1
  m2 = runTSMoney tsm2
  in RM.add m1 m2
runTSMoney (Conv v tsm) = let
  m = runTSMoney tsm
  in RM.mul m v

module Money.TypeSafe.Money where

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

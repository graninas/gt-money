module Money.Raw.Money where



-- Questions:
-- Precision
-- Parsing
-- Overflow and underflow
-- Currency
-- UX



data Money = Money Int
  deriving (Show, Read, Eq, Ord)


add :: Money -> Money -> Money
add (Money m1) (Money m2) = Money $ m1 + m2

zero :: Money
zero = Money 0

-- Opt 1

data Currency
  = EUR
  | USD
  deriving (Show, Read, Eq, Ord)

data CMoney = CMoney Currency Money
  deriving (Show, Read, Eq, Ord)

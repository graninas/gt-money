module Money.Raw.Money where



-- Questions:
-- - Precision
-- - Parsing
-- - Overflow and underflow
-- - Currency
-- - UX
-- - What is interface and implementation?
-- - What is implementation details?



data Money = Money Int
  deriving (Show, Read, Eq, Ord)

mkMoney :: Int -> Money
mkMoney = Money

add :: Money -> Money -> Money
add (Money m1) (Money m2) = Money $ m1 + m2

mul :: Money -> Int -> Money
mul (Money m1) v = Money $ m1 * v

zero :: Money
zero = Money 0

-- Opt 1

-- Closed currency category. Only this type can be a currency, with a limited set of currencies.

data Currency
  = EUR
  | USD
  deriving (Show, Read, Eq, Ord)

data CMoney = CMoney Currency Money
  deriving (Show, Read, Eq, Ord)

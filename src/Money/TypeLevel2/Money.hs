{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE UndecidableInstances #-}

module Money.TypeLevel2.Money where

import           Data.Map (Map)
import           Data.Proxy (Proxy(..))

import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol)



data EnglishAuction holder exchangeService (lots :: [ LotTag ])

data Holder (name :: Symbol)

data ExchangeService (name :: Symbol)

-- This data type generates implicitly:
--   kind AcceptTag
--   type AcceptTag (of kind AcceptTag)   -- compatibility with previous GHC
--   type 'AcceptTag (of kind AcceptTag)
data AcceptTag = AcceptTag

data LotTag = LotTag

-- A way to produce list of types of a predefined kind
-- (We produce list of types of kind AcceptTag).
--
-- This can be a base trick for all the type level eDSLs.

-- This design allows nonsense: Accept Bool.
-- We defer checking to the interpretation phase.

type family Accept (a :: *) :: AcceptTag

type family Lot (name :: Symbol) (descr :: Symbol) (accepts :: [ AcceptTag ]) :: LotTag

-- Type safe currency

class Currency cur

-- Implementation

class Eval tag payload res where
  eval :: Proxy tag -> Proxy payload -> IO res

instance Eval AcceptTag accepts (Map Name Amount) => Eval LotTag accepts (Map Name Amount) where
  eval = undefined


-- Type safety: you can't have an empty list of accepted currencies.
-- instance Eval AcceptTag '[] () where
  -- eval _ _ = undefined

instance Eval AcceptTag (a ': '[]) () where
  eval _ _ = undefined

instance Eval AcceptTag (b ': a ': as) () where
  eval _ _ = undefined

-- doesn't work: duplicated instances
-- instance (a ~ Accept USD) => Eval a () () where
--   eval _ _ = pure ()
--
-- instance (a ~ Accept EUR) => Eval a () () where
--   eval _ _ = pure ()

-- tmp tests
type SomeLot = Lot "a" "b" (Accept USD ': Accept EUR ': '[])

someFunc6 :: IO ()
someFunc6 = eval (Proxy :: Proxy AcceptTag) (Proxy :: Proxy (Accept EUR ': Accept USD ': '[]))

-- Type safety: you can't have an empty list of accepted currencies.
-- someFunc5 :: IO ()
-- someFunc5 = eval (Proxy :: Proxy AcceptTag) (Proxy :: Proxy '[])

someFunc4 :: IO ()
someFunc4 = eval (Proxy :: Proxy AcceptTag) (Proxy :: Proxy (Accept EUR ': Accept USD ': '[]))

someFunc3 :: IO ()
someFunc3 = eval (Proxy :: Proxy AcceptTag) (Proxy :: Proxy (Accept EUR ': '[]))

-- Useless
-- instance Eval '[] b () where
--   eval _ _ = pure ()
-- someFunc2 :: IO ()
-- someFunc2 = eval (Proxy :: Proxy ('[])) (Proxy :: Proxy ())

-- Redundant
-- instance (a ~ Accept cur, Currency cur) => Eval a cur () where
--   eval _ _ = pure ()
-- someFunc1 :: IO ()
-- someFunc1 = eval (Proxy :: Proxy (Accept EUR)) (Proxy :: Proxy EUR)

-- instance Eval accepts (Map Name Amount) => Eval (Lot name descr accepts) (Map Name Amount) where
--   eval = undefined
--
-- instance Eval lots (Map Name Amount) => Eval (EnglishAuction holder exch lots) AuctionsResult where
--   eval = undefined

type Name = String
type Amount = Integer
data AuctionsResult = AuctionsResult
  { sold :: Map Name Amount
  }

-- runAuctions :: Proxy Auctions -> IO AuctionsResult
-- runAuctions aus = eval aus


-- User space

data USD
data EUR

-- Finely composable with a type safe approach
instance Currency EUR

-- Question: can Accept USD and Accept EUR be distinguished on interpreting?

type Auctions =
  EnglishAuction
    ( Holder "UK Bank" )
    ( ExchangeService "UK Bank" )
    (  Lot "a" "b" (Accept USD ': Accept EUR ': '[])
    ': Lot "302" "Dali artwork" (Accept USD ': Accept EUR ': '[])
    ': Lot "403" "Ancient mechanism" (Accept USD ': '[])
    ': '[]
    )



-- Idea: separation of type safe interpretation and liberal (not type safe) eDSL definition.
-- eDSL definition: allows extensions, doesn't restrict on extension points.
-- Interpretatoin: restricts the extension points.
-- See currency.

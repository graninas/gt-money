{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeInType         #-}

module Money.TypeLevel2.Money where



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



-- User space

data USD
data EUR

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

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeInType         #-}

module Money.TypeLevel2.Money where



import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol)



data EnglishAuction holder exchangeService (lots :: [ LotTag ])

data Holder name

data ExchangeService name

-- This data type generates implicitly:
--   kind AcceptTag
--   type AcceptTag (of kind AcceptTag)   -- compatibility with previous GHC
--   type 'AcceptTag (of kind AcceptTag)
data AcceptTag = AcceptTag

data LotTag = LotTag

data CurrencyTag = CurrencyTag

data MkCurrencyTag = MkCurrencyTag


-- A way to produce list of types of a predefined kind
-- (We produce list of types of kind AcceptTag).
--
-- This can be a base trick for all the type level eDSLs.
--
--
-- Accept :: CurrencyTag -> AcceptTag
-- A way to produce something of AcceptTag kind.
--
-- This is an opened Type Family which accepts any type with a kind CurrencyTag.
-- A type cannot be made of the kind CurrencyTag explicitly (unless there is some trik in GHC),
-- but a type family Currency can produce such tag.
-- It's an open type family, so we can add our own currencies.
-- However we should now construct the AcceptTag as Accept (Currency USD). This doesn't work:
-- type instance Accept (Currency USD) = 'AcceptTag

type family MkCurrency (a :: *) :: MkCurrencyTag

type family Currency (a :: MkCurrencyTag) :: CurrencyTag

type family Accept (a :: CurrencyTag) :: AcceptTag

type family Lot (name :: Symbol) (descr :: Symbol) (accepts :: [ AcceptTag ]) :: LotTag

data USD = USD
data EUR = EUR
data UsdTag = UsdTag
data EurTag = EurTag


-- type instance Currency (MkCurrency EUR) = 'CurrencyTag

type instance Accept a = 'AcceptTag


-- Question: can Accept 'USD and Accept 'EUR be distinguished on interpreting?

-- type Auctions =
--   EnglishAuction
--     ( Holder "UK Bank" )
--     ( ExchangeService "UK Bank" )
--
--     -- Nonsense
--     (  Lot "a" "b" (Accept (Currency USD) ': Accept (Currencies EUR) ': Accept (Currencies Bool) ': '[])
--     -- ': Lot "302" "Dali picture" (Accept (Currency USD) ': Accept (Currency EUR) ': '[])
--     -- ': Lot "403" "Ancient mechanism" (Accept (Currency USD) ': '[])
--     ': '[]
--     )

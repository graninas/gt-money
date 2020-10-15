{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeInType         #-}

module Money.TypeLevel2.Money where



import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol)


-- | Capture a value from the request path under a certain type @a@.
--
-- Example:
--
-- >>>            -- GET /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
type Capture = Capture' '[] -- todo

-- | 'Capture' which can be modified. For example with 'Description'.
data Capture' (mods :: [*]) (sym :: Symbol) (a :: *)
    deriving (Typeable)


-- class Currency cur
--
-- data USD
-- data EUR

data Currency = USD | EUR

-- kind Currency
-- type USD :: Currency
-- type EUR :: Currency


data EnglishAuction holder exchangeService lots lots2

data Holder name

data ExchangeService name

data Accept (cur :: Currency)
data Accept2 = Accept2

data Lot (name :: Symbol) (descr :: Symbol) (accepts :: [ Currency ] )

-- A way to produce list of types of a predefined kind
-- (We produce list of types of kind Accept2).

-- This can be a base trick for all the type level eDSLs.
data Lot2 (name :: Symbol) (descr :: Symbol) (accepts :: [ Accept2 ] )

-- AcceptTF :: Currency -> Accept2
type family AcceptTF (a :: Currency) :: Accept2

type instance AcceptTF 'USD = 'Accept2
type instance AcceptTF 'EUR = 'Accept2


type Auctions =
  EnglishAuction
    ( Holder "UK Bank" )
    ( ExchangeService "UK Bank" )
    ( Lot "201" "Chinesse vase" ( 'USD ': '[] )
    -- ': Lot "302" "Dali picture" (Accept 'USD ': Accept 'EUR ': '[])
    -- ': Lot "403" "Ancient mechanism"  (Accept 'USD ': '[])
    ': '[]
    )

    ( Lot2 "a" "b" (AcceptTF 'USD ': AcceptTF 'EUR ': '[])
    )




--- What does this code mean??
-- type family AcceptTF a
-- data Lot' (name :: Symbol) (descr :: Symbol) (accepts :: [ AcceptTF * ])
-- type instance AcceptTF (Accept' USD) = ()

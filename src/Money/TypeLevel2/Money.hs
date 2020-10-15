{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}

module Money.TypeLevel2.Money where

import qualified Money.Raw.Money as RM


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


data EnglishAuction holder exchangeService lots

data Holder name

data ExchangeService name

data Accept' (cur :: Currency)

-- This allows to specify too much stuff.
data Lot' (name :: Symbol) (descr :: Symbol) (accepts :: [ * ])


type Lot = Lot'
type Accept = Accept'

data Nonsense


type Auctions =
  EnglishAuction
    ( Holder "UK Bank" )
    ( ExchangeService "UK Bank" )
    (  Lot "201" "Chinesse vase" ( Accept 'USD ': Nonsense ': '[])
    -- ': Lot "302" "Dali picture" (Accept 'USD ': Accept 'EUR ': '[])
    -- ': Lot "403" "Ancient mechanism"  (Accept 'USD ': '[])
    ': '[]
    )




--- What does this code mean??
-- type family AcceptTF a
-- data Lot' (name :: Symbol) (descr :: Symbol) (accepts :: [ AcceptTF * ])
-- type instance AcceptTF (Accept' USD) = ()

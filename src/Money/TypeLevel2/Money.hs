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


data EnglishAuction holder exchangeService (lots :: [ LotTag ])

data Holder name

data ExchangeService name

-- This data type generates implicitly:
--   kind AcceptTag
--   type AcceptTag (of kind AcceptTag)   -- compatibility with previous GHC
--   type 'AcceptTag (of kind AcceptTag)
data AcceptTag = AcceptTag

data LotTag = LotTag


-- A way to produce list of types of a predefined kind
-- (We produce list of types of kind AcceptTag).

-- This can be a base trick for all the type level eDSLs.


-- Accept :: Currency -> AcceptTag
-- A way to produce something of AcceptTag kind.
--
-- This is an opened Type Family, but we can't add a new currency because
-- we're limited by the closed data type Currency.
-- So this type family cannot be considered 'opened' in the extensibility sense,
-- only in the syntactical sense.
type family Accept (a :: Currency) :: AcceptTag

type instance Accept 'USD = 'AcceptTag
type instance Accept 'EUR = 'AcceptTag


type family Lot (name :: Symbol) (descr :: Symbol) (accepts :: [ AcceptTag ]) :: LotTag



-- Question: can Accept 'USD and Acceept 'EUR be distinguished on interpreting?


type Auctions =
  EnglishAuction
    ( Holder "UK Bank" )
    ( ExchangeService "UK Bank" )
    (  Lot "a" "b" (Accept 'USD ': Accept 'EUR ': '[])
    ': Lot "302" "Dali picture" (Accept 'USD ': Accept 'EUR ': '[])
    ': Lot "403" "Ancient mechanism" (Accept 'USD ': '[])
    ': '[]
    )




--- What does this code mean??
-- type family AcceptTF a
-- data Lot' (name :: Symbol) (descr :: Symbol) (accepts :: [ AcceptTF * ])
-- type instance AcceptTF (Accept' USD) = ()

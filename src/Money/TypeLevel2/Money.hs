{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Money.TypeLevel2.Money where

import           Data.Map (Map)
import           Data.Proxy (Proxy(..))

import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol)

-- eDSL

data Car (name :: Symbol) (engine :: EngineTag x) (parts :: PartsTag a)

data EngineTag a

data PartsTag a

-- Construction of extensions

type family Engine (a :: *) :: EngineTag a

type family Parts (p :: [*]) :: PartsTag p

-- type family NoParts :: PartsTag '[]

-- Implementation

class Eval tag payload res where
  eval :: tag -> Proxy payload -> IO res

-- Interpreting of the (engine :: EngineTag x)
data AsEngine = AsEngine

instance Eval AsEngine FusionMkI () where
  eval _ _ = undefined

instance Eval AsEngine BrokenEngine () where
  eval _ _ = undefined

instance (b ~ Engine a, Eval AsEngine a ()) => Eval AsEngine b () where
  eval _ _ = eval AsEngine (Proxy :: Proxy a)

instance Eval AsEngine engine () => Eval AsEngine (Car name engine parts) () where
  eval _ _ = eval AsEngine (Proxy :: Proxy engine)




-- Interpreting of the (parts :: PartsTag a)

data AsPart = AsPart

instance Eval AsPart FusionMkI () where
  eval _ _ = undefined

instance Eval AsPart BrokenEngine () where
  eval _ _ = undefined

instance Eval AsPart '[] () where
  eval _ _ = pure ()

instance Eval AsPart p () => Eval AsPart (p ': '[]) () where
  eval _ _ = eval AsPart (Proxy :: Proxy p)

instance (Eval AsPart p (), Eval AsPart (x ': ps) ()) => Eval AsPart (p ': x ': ps) () where
  eval _ _ = do
    () <- eval AsPart (Proxy :: Proxy p)
    () <- eval AsPart (Proxy :: Proxy (x ': ps))
    pure ()

instance (b ~ Parts a, Eval AsPart a ()) => Eval AsPart b () where
  eval _ _ = eval AsPart (Proxy :: Proxy a)




-- user space


data FusionMkI
data BrokenEngine

type MyCar1 = Car "A" (Engine FusionMkI) (Parts '[])
type MyCar2 = Car "B" (Engine FusionMkI) (Parts '[FusionMkI])
type MyCar3 = Car "C" (Engine FusionMkI) (Parts '[FusionMkI, BrokenEngine])


runner :: IO ()
runner = do
  () <- eval AsEngine (Proxy :: Proxy (Engine FusionMkI))
  () <- eval AsEngine (Proxy :: Proxy (Engine BrokenEngine))
  -- () <- eval (Proxy :: Proxy MyCar1)
  () <- eval AsPart (Proxy :: Proxy (Parts '[]))
  () <- eval AsPart (Proxy :: Proxy (Parts '[FusionMkI]))
  () <- eval AsPart (Proxy :: Proxy (Parts '[FusionMkI, BrokenEngine]))

  pure ()




-- Type safety: you can't have an empty list of accepted currencies.
-- instance Eval AcceptTag '[] () where
  -- eval _ _ = undefined

-- instance Eval AcceptTag (a ': '[]) () where
--   eval _ _ = undefined
--
-- instance Eval AcceptTag (b ': a ': as) () where
--   eval _ _ = undefined

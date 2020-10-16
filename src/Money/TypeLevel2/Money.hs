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

data Car (name :: Symbol) (engine :: EngineTag x) (parts :: PartsTag a )

data EngineTag a

data PartsTag a

-- Construction of extensions

type family Engine (a :: *) :: EngineTag a

type family Parts (p :: [*]) :: PartsTag p

-- type family NoParts :: PartsTag '[]

-- Implementation

class Eval payload res where
  eval :: Proxy payload -> IO res


instance Eval FusionMkI () where
  eval _ = undefined

instance Eval BrokenEngine () where
  eval _ = undefined

instance (b ~ Engine a, Eval a ()) => Eval b () where
  eval _ = eval (Proxy :: Proxy a)

instance Eval engine () => Eval (Car name engine parts) () where
  eval _ = eval (Proxy :: Proxy engine)


instance Eval '[] () where
  eval _ = pure ()

instance Eval p () => Eval (p ': '[]) () where
  eval _ = eval (Proxy :: Proxy p)

instance (Eval p (), Eval (x ': ps) ()) => Eval (p ': x ': ps) () where
  eval _ = do
    () <- eval (Proxy :: Proxy p)
    () <- eval (Proxy :: Proxy (x ': ps))
    pure ()

instance (b ~ Parts a, Eval a ()) => Eval b () where
  eval _ = eval (Proxy :: Proxy a)




-- user space


data FusionMkI
data BrokenEngine

type MyCar1 = Car "A" (Engine FusionMkI) (Parts '[])
type MyCar2 = Car "B" (Engine FusionMkI) (Parts '[FusionMkI])
type MyCar3 = Car "C" (Engine FusionMkI) (Parts '[FusionMkI, BrokenEngine])


runner :: IO ()
runner = do
  () <- eval (Proxy :: Proxy (Engine FusionMkI))
  () <- eval (Proxy :: Proxy (Engine BrokenEngine))
  () <- eval (Proxy :: Proxy MyCar1)
  () <- eval (Proxy :: Proxy (Parts '[]))
  () <- eval (Proxy :: Proxy (Parts '[FusionMkI]))
  () <- eval (Proxy :: Proxy (Parts '[FusionMkI, BrokenEngine]))

  pure ()




-- Type safety: you can't have an empty list of accepted currencies.
-- instance Eval AcceptTag '[] () where
  -- eval _ _ = undefined

-- instance Eval AcceptTag (a ': '[]) () where
--   eval _ _ = undefined
--
-- instance Eval AcceptTag (b ': a ': as) () where
--   eval _ _ = undefined

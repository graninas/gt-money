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

data Car (name :: Symbol) (engine :: EngineTag x)

data EngineTag x = EngineTag

type family Engine (a :: *) :: EngineTag a

-- Implementation

class Eval tag payload res where
  eval :: tag -> Proxy payload -> IO res


instance Eval () FusionMkI () where
  eval _ _ = undefined

instance Eval () BrokenEngine () where
  eval _ _ = undefined

instance (b ~ Engine a, Eval () a ()) => Eval () b () where
  eval _ _ = eval () (Proxy :: Proxy a)



-- user space


data FusionMkI
data BrokenEngine

type MyCar1 = Car "A" (Engine FusionMkI)
type MyCar2 = Car "A" (Engine BrokenEngine)


runner :: IO ()
runner = do
  () <- eval () (Proxy :: Proxy (Engine FusionMkI))
  () <- eval () (Proxy :: Proxy (Engine BrokenEngine))
  pure ()




-- Type safety: you can't have an empty list of accepted currencies.
-- instance Eval AcceptTag '[] () where
  -- eval _ _ = undefined

-- instance Eval AcceptTag (a ': '[]) () where
--   eval _ _ = undefined
--
-- instance Eval AcceptTag (b ': a ': as) () where
--   eval _ _ = undefined

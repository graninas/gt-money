-- {-# LANGUAGE DataKinds          #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE PolyKinds          #-}
-- {-# LANGUAGE TypeInType         #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies          #-}

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

-- Implementation

-- This FunDep is needed to simplify the return type inference.
class Eval tag payload ret | tag payload -> ret where
  eval :: tag -> Proxy payload -> IO ret

-- Interpreting of the (engine :: EngineTag x)
data AsEngine = AsEngine

instance Eval AsEngine FusionMkI () where
  eval _ _ = putStrLn "Engine: FusionMkI"

instance Eval AsEngine BrokenEngine () where
  eval _ _ = putStrLn "Engine: BrokenEngine"

instance (b ~ Engine a, Eval AsEngine a ()) => Eval AsEngine b () where
  eval _ _ = eval AsEngine (Proxy :: Proxy a)



-- Interpreting of the (parts :: PartsTag a)

data AsPart = AsPart

instance Eval AsPart FusionMkI () where
  eval _ _ = putStrLn "Part: FusionMkI"

instance Eval AsPart BrokenEngine () where
  eval _ _ = putStrLn "Part: BrokenEngine"

instance Eval AsPart '[] () where
  eval _ _ = pure ()

instance Eval AsPart p () => Eval AsPart (p ': '[]) () where
  eval _ _ = eval AsPart (Proxy :: Proxy p)

instance (Eval AsPart p (), Eval AsPart (x ': ps) ()) => Eval AsPart (p ': x ': ps) () where
  eval _ _ = do
    eval AsPart (Proxy :: Proxy p)
    eval AsPart (Proxy :: Proxy (x ': ps))

instance (b ~ Parts a, Eval AsPart a ()) => Eval AsPart b () where
  eval _ _ = eval AsPart (Proxy :: Proxy a)

-- Interpreting of the Car

data AsCar = AsCar

instance (Eval AsEngine engine (), Eval AsPart parts ()) =>
  Eval AsCar (Car name engine parts) () where
  eval _ _ = do
    putStrLn "This is a car."
    eval AsEngine (Proxy :: Proxy engine)
    eval AsPart (Proxy :: Proxy parts)


-- user space


data FusionMkI
data BrokenEngine

type MyCar1 = Car "A" (Engine FusionMkI) (Parts '[])
type MyCar2 = Car "B" (Engine FusionMkI) (Parts '[FusionMkI])
type MyCar3 = Car "C" (Engine FusionMkI) (Parts '[FusionMkI, BrokenEngine])

runner :: IO ()
runner = do
  eval AsEngine (Proxy :: Proxy (Engine FusionMkI))
  eval AsEngine (Proxy :: Proxy (Engine BrokenEngine))
  eval AsPart (Proxy :: Proxy (Parts '[]))
  eval AsPart (Proxy :: Proxy (Parts '[FusionMkI]))
  eval AsPart (Proxy :: Proxy (Parts '[FusionMkI, BrokenEngine]))
  eval AsCar (Proxy :: Proxy MyCar3)

-- Output:
-- Engine: FusionMkI
-- Engine: BrokenEngine
-- Part: FusionMkI
-- Part: FusionMkI
-- Part: BrokenEngine
-- This is a car.
-- Engine: FusionMkI
-- Part: FusionMkI
-- Part: BrokenEngine

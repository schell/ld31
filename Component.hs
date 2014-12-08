{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Component where

import Types
import Yarn
import Control.Lens
import Data.IntMap
import Data.Typeable
import Data.Maybe
import Control.Eff
import Control.Eff.State.Strict

deriving instance Typeable Identity

type Var a = Yarn Identity () a
type Component a = IntMap a

data Dynamic a = Dynamic { dynVar :: Maybe (Var a)
                         , dynVal :: a
                         } deriving (Typeable)
makeLensesFor [("dynVar", "_dynVar")
              ,("dynVal", "_dynVal")
              ] ''Dynamic

type DynamicValue a = IntMap (Dynamic a)

static :: a -> Dynamic a
static a = Dynamic { dynVar = Nothing
                   , dynVal = a
                   }

dynamic :: Yarn Identity () a -> Dynamic a
dynamic y = static a & _dynVar .~ Just y'
    where Output a y' = runIdentity $ stepYarn y (0 :: Float) ()

stepDynamic :: Float -> Dynamic a -> Dynamic a
stepDynamic dt dyn
    | Nothing <- (dyn ^. _dynVar) = dyn
    | otherwise = dyn & _dynVar .~ Just y' & _dynVal .~ a'
        where Output a' y' = runIdentity $ stepYarn (fromJust $ dyn ^. _dynVar) dt ()

stepComponent :: Float -> DynamicValue a -> DynamicValue a
stepComponent dt c = fmap (stepDynamic dt) c

startVaryingWith :: Dynamic a -> Yarn Identity () a -> Dynamic a
startVaryingWith dyn y = dyn & _dynVar .~ Just y

stopVarying :: Dynamic a -> Dynamic a
stopVarying dyn = dyn & _dynVar .~ Nothing

getVals :: (Member (State (DynamicValue a)) r, Typeable a)
        => Eff r (Component a)
getVals = do
    (intmap :: DynamicValue a) <- get
    return $ fmap dynVal intmap

getVars :: (Member (State (DynamicValue a)) r, Typeable a)
        => Eff r (Component (Maybe (Var a)))
getVars = do
    (intmap :: DynamicValue a) <- get
    return $ fmap dynVar intmap

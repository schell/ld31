{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Entity where

import Types
import Component
import Data.Typeable
import Data.IntMap
import qualified Data.Map as M
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.State.Strict

data CompositeEntity = CompositeEntity (M.Map String ID)
                     | MetaEntity (M.Map String CompositeEntity)

type CanHas a r = Member (Component a) r
type CanHasTween a r = Member (State (IntMap (Dynamic a))) r
type CanMakeThings r = Member (Fresh ID) r

hasProperty :: (Member (State (IntMap a)) r, Typeable a) => ID -> a -> Eff r ()
hasProperty eid val = modify $ insert (unID eid) val

(##) :: (Member (State (IntMap a)) r, Typeable a) => Eff r ID -> a -> Eff r ID
f ## prop = do
   eid <- f
   modify $ insert (unID eid) prop
   return eid

(.#) :: (Member (State (IntMap a)) r, Typeable a) => Eff r ID -> a -> Eff r ()
f .# prop = do
   eid <- f
   modify $ insert (unID eid) prop
   return ()

(-#) :: (Member (State (IntMap (Dynamic a))) r, Typeable a)
     => Eff r ID -> a -> Eff r ID
f -# prop = f ## static prop

(.-#) :: (Member (State (IntMap (Dynamic a))) r, Typeable a)
      => Eff r ID -> a -> Eff r ()
f .-# prop = f .# static prop

(~#) :: (Member (State (IntMap (Dynamic a))) r, Typeable a)
     => Eff r ID -> Var a -> Eff r ID
f ~# prop = f ## dynamic prop

(.~#) :: (Member (State (IntMap (Dynamic a))) r, Typeable a)
      => Eff r ID -> Var a -> Eff r ()
f .~# prop = f .# dynamic prop

entity :: (Member (Fresh a) r, Enum a, Typeable a) => Eff r a
entity = fresh

getEntityByName :: (Member (State (M.Map String ID)) r)
                => String -> Eff r (Maybe ID)
getEntityByName name = do
    m <- get
    return $ M.lookup name m

putEntityName :: (Member (State (M.Map String ID)) r)
              => String -> ID -> Eff r ()
putEntityName n i = modify $ (M.insert n i)

isNamed :: (Member (State (M.Map String ID)) r)
        => ID -> String -> Eff r ()
isNamed = flip putEntityName

intersectionWith3 f a b c = intersectionWith ($) (intersectionWith f a b) c
intersectionWith4 f a b c d = intersectionWith ($) (intersectionWith3 f a b c) d
intersectionWith5 f a b c d e = intersectionWith ($) (intersectionWith4 f a b c d) e


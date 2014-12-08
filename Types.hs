{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types where

import Linear
import Control.Lens
import Data.Typeable

newtype ID = ID { unID :: Int } deriving (Show, Read, Eq, Ord, Typeable, Enum, Num)
newtype Player = Player ID deriving (Typeable)

type Color = V4 Float
type Width = Float
type Height = Float
type HalfWidth = Float
type HalfHeight = Float
type SeparatingAxis = V2 Float
type Line = (V2 Float, V2 Float)
newtype Size = Size (V2 Float) deriving (Typeable, Show, Eq, Ord)

unsize :: Size -> V2 Float
unsize (Size v) = v

newtype Position = Position (V2 Float) deriving (Typeable, Show, Eq, Ord)

unpos :: Position -> V2 Float
unpos (Position v) = v

newtype Velocity = Velocity (V2 Float) deriving (Typeable)

unvel :: Velocity -> V2 Float
unvel (Velocity v) = v

newtype Name = Name String deriving (Show, Typeable)
newtype PositionOffset = PositionOffset (V2 Float) deriving (Typeable)

unposoff :: PositionOffset -> V2 Float
unposoff (PositionOffset v) = v

newtype Rotation = Rotation Float deriving (Typeable, Show, Eq, Ord)

unrot :: Rotation -> Float
unrot (Rotation r) = r

newtype Scale = Scale (V2 Float) deriving (Show, Eq, Typeable)

unscl :: Scale -> V2 Float
unscl (Scale v) = v

data AABB = AABB { aabbCenter     :: V2 Float
                 , aabbHalfVector :: V2 Float
                 } deriving (Show, Eq, Ord, Typeable)
makeLensesFor [("aabbCenter", "aabbCenterLens")
              ,("aabbHalfVector", "aabbHalfVectorLens")
              ] ''AABB

data Mass = Kilos Float
          | Immobile
          deriving (Typeable, Eq, Ord, Show)

massOf :: Mass -> Float
massOf (Kilos k) = k
massOf _ = 0

data PhysicalBody = PhysicalBody { pbAABB :: AABB
                                 , pbMass :: Mass
                                 } deriving (Typeable, Eq, Ord, Show)
makeLensesFor [("pbAABB", "pbAABBLens")
              ,("pbMass", "pbMassLens")
              ] ''PhysicalBody


isMobile :: PhysicalBody -> Bool
isMobile (PhysicalBody _ Immobile) = False
isMobile _ = True

data Quadtree a = Quadtree { qtBounds :: AABB
                           , qtItems  :: [(AABB, a)]
                           , qtQuads  :: Maybe ( Quadtree a
                                               , Quadtree a
                                               , Quadtree a
                                               , Quadtree a
                                               )
                           } deriving (Show, Eq, Ord, Typeable)

data TimeDelta = Float
data Direction = Up
               | Down
               | Left'
               | Right'
               | None
               deriving (Eq, Ord, Show, Typeable)

data Path = PathVertical
          | PathHorizontal
          | PathOpen
          deriving (Eq)

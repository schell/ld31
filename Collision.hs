module Collision where

import Prelude hiding (sequence_, minimum)
import Gelatin hiding (drawArrays, get, Position, renderer, Name)
import Types
import Data.Foldable
import Control.Applicative
import Control.Lens

aabbAxes :: [SeparatingAxis]
aabbAxes =
    [ V2 1 0
    , V2 0 1
    ]

aabbLines :: AABB -> [Line]
aabbLines a = [(tl, tr), (tr, br), (br, bl), (bl, tl)]
    where [tl,tr,bl,br] = aabbPoints a

aabbPoints :: AABB -> [V2 Float]
aabbPoints (AABB (V2 x y) (V2 hw hh)) = [V2 l t, V2 r t, V2 l b, V2 r b]
    where (l,t,r,b) = (x - hw, y - hh, x + hw, y + hh)

aabbProjectionRange :: AABB -> V2 Float -> (Float, Float)
aabbProjectionRange aabb axis = range
    where range = Prelude.foldl (\(n,x) p' -> (min n p', max x p'))
                                (1/0, -(1/0))
                                prjs
          ps    = aabbPoints aabb
          prjs  = map (`dot` axis) ps

collidesWithRange :: (Float, Float) -> (Float, Float) -> Bool
collidesWithRange (a,b) (c,d) = not (d <= a || b <= c)

collideOnAxis :: AABB -> AABB -> V2 Float -> Maybe (V2 Float)
collideOnAxis a b axis = if col then Just $ u ^* v else Nothing
    where rA@(n1, n2) = aabbProjectionRange a axis
          rB@(m1, m2) = aabbProjectionRange b axis
          v = min (m2 - n1) (n2 - m1)
          d = (a^.aabbCenterLens) ^-^ (b^.aabbCenterLens)
          s = (\n -> if n >= 0 then 1 else (-1)) <$> d
          u = s * signorm axis
          col = collidesWithRange rA rB

collidedInto :: AABB -> AABB -> Maybe (V2 Float)
collidedInto a b = (minimumBy dv) <$> (sequence axisOverlaps)
    where axisOverlaps = map (collideOnAxis a b) aabbAxes
          dv v1 v2 = compare (norm v1) (norm v2)

collideBodies :: (PhysicalBody, PhysicalBody) -> (PhysicalBody, PhysicalBody)
collideBodies (a, b)
    | (not $ isMobile a) && (not $ isMobile b) = (a, b)
    | otherwise = (a', b')
        where p1 = a ^. pbAABBLens.aabbCenterLens
              p2 = b ^. pbAABBLens.aabbCenterLens
              mv = (pbAABB a) `collidedInto` (pbAABB b)
              ma = (massOf $ pbMass a) / tm
              mb = (massOf $ pbMass b) / tm
              (va,vb) = if isMobile a && isMobile b
                         then (1 - ma, 1 - mb)
                         else if isMobile a
                                then (1,0)
                                else (0,1)
              tm = (massOf $ pbMass a) + (massOf $ pbMass b)
              p1' = maybe p1 ((p1 ^+^) . (va *^)) mv
              p2' = maybe p2 ((p2 ^-^) . (vb *^)) mv
              a' = a & pbAABBLens.aabbCenterLens .~ p1'
              b' = b & pbAABBLens.aabbCenterLens .~ p2'

foldCollision :: (Int, PhysicalBody) -> [(Int, PhysicalBody)] -> [(Int, PhysicalBody)]
foldCollision p [] = [p]
foldCollision (p, pb) ((i, b):bs)
    | p == i = foldCollision (p, pb) bs
    | otherwise = (i, b') : foldCollision (p, pb') bs
        where (pb', b') = collideBodies (pb, b)

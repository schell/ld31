{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
module Main where

import Prelude hiding (sequence_, minimum)
import Gelatin hiding (drawArrays, get, Position, renderer, Name, Scale, Size)
import Entity
import Component
import Yarn
import Types
import Toons
import UserInput
import Rendering
import Collision
import Data.Time.Clock
import Data.Typeable
import Data.Monoid
import Data.Foldable
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Control.Monad.Reader as R
import Control.Lens
import Control.Concurrent
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Control.Applicative
import System.Exit
import System.Random


textbox :: ( CanHasTween Displayable r, CanHasTween Colors r
           , CanHasTween Position r, CanHasTween Scale r
           , CanHasTween Rotation r, CanMakeThings r)
        => V2 Float -> String -> Eff r CompositeEntity
textbox pos str = do
    let (cw, ch) = (s*5,s*7)
        s = 1.6
        lns = lines str
        sx = Prelude.maximum $ map ((cw *) . fromIntegral . length) lns
        sy = ch * (fromIntegral $ length lns)
    frame <- entity -# FancyBox
                    -# (Colors gray transparent)
                    -# (Position pos)
                    -# (Scale $ V2 (sx/2 + 4) (sy/2 + 4))
                    -# (Rotation 0)

    text <- entity -# Text str
                   -# (Colors gray transparent)
                   -# (Position $ pos + V2 (-sx/2 + 4) (-sy/2 + 8))
                   -# (Scale $ V2 s s)
                   -# (Rotation 0)
    return $ CompositeEntity $ M.fromList [("frame",frame), ("text",text)]

initialize = do
    tb <- textbox (V2 300 300) "Once there existed a ghost..."

    let blockPos = Position $ V2 150 150
    entity -# Box
           -# (Colors transparent green)
           -# blockPos
           -# (Size $ V2 20 20)
           -# (Kilos 50)
           -# (Rotation 0)
          .-# (Scale $ V2 40 40)

    entity -# Moon
           -# (Position $ V2 550 40)
           -# (Scale $ V2 1.75 1.75)
           -# (Rotation 0)
          .-# (Colors gray transparent)

    forM_ [1..4] $ \i -> do
        s <- lift $ randomRIO (0.75,3)
        x <- lift $ randomRIO (-20,0)
        y <- lift $ randomRIO (0,20)
        n <- lift $ randomRIO (10,50)
        let varx = linear 0 100 (n :: Float) `andThen` varx
            varP = PositionOffset <$> (V2 <$> varx <*> 0)
        entity -# (Cloud i)
               -# (Position $ V2 (500 + x) (50 + y))
               ~# varP
               -# (Scale $ V2 s s)
               -# (Rotation 0)
              .-# (Colors black transparent)

    let playerPos = Position $ V2 150 200
        hovering  = PositionOffset <$> (V2 <$> 0 <*> (updown :: Var Float))
        updown    = linear 0 (-5) (1 :: Float) `andThen`
                      linear (-5) 0 (1 :: Float) `andThen` updown

    player <- entity -# (Reaper East)
                     -# (Rotation 0)
                     -# Colors pink transparent
                     -# playerPos
                     -# (Size $ V2 5 14)
                     -# (Kilos 10)
                     -# (Scale $ V2 1 1)
                     -# playerPos
                     ~# hovering
                     ## (playerDirection 150 dpadDirectionMap)
                     ## (playerDirection 150 wasdDirectionMap)
    player `isNamed` "player"

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
-- Find entities that have keyboard control and progress their positions.
stepKeyboardControlledThings dt = do
    (input :: InputEnv) <- get
    (positions :: Component Position) <- getVals
    (controls :: Component (PlayerDirection Key)) <- get

    let positions' = IM.intersectionWith (\ctrl pos -> Position $
                                            unvel (ctrl (ienvKeysDown input) dt) + unpos pos)
                                         controls
                                         positions
    modify $ IM.union $ fmap static positions'

-- Find entities that have joystick control and progress their positions.
stepJoystickControlledThings dt = do
    mJInput <- lift $ getJoystickInput Joystick'1
    let f s (b, i) = if b == JoystickButtonState'Pressed then S.insert i s else s
        buttonSet = case mJInput of
                        Nothing -> S.empty
                        Just ji -> Prelude.foldl f S.empty $ zip (jiButtons ji) [0..]
    --lift $ print buttonSet
    (positions :: Component Position) <- getVals
    (poscontrols :: Component (PlayerDirection Int)) <- get

    (rotations :: Component Rotation) <- getVals
    (rotcontrols :: Component (PlayerRotation Int)) <- get

    let positions' = IM.intersectionWith (\ctrl pos -> Position $
                                            unvel (ctrl buttonSet dt) + unpos pos)
                                         poscontrols
                                         positions
        rotations' = IM.intersectionWith (\ctrl rot -> Rotation $
                                            unrot (ctrl buttonSet dt) + unrot rot)
                                         rotcontrols
                                         rotations
    modify $ IM.union $ fmap static positions'
    modify $ IM.union $ fmap static rotations'

-- Collide just the player with the environment.
collidePlayer = do
    Just (ID player) <- getEntityByName "player"
    positions <- getVals
    sizes     <- getVals
    masses    <- getVals
    let aabbs = IM.intersectionWith (\(Position p) (Size s) -> AABB p s)
                                    positions
                                    sizes
        bodies = IM.intersectionWith PhysicalBody aabbs masses
        mposis = do pbody <- IM.lookup player bodies
                    let bodies' = foldCollision (player, pbody) $
                                    IM.toList bodies

                    return $ fmap (^.pbAABBLens.aabbCenterLens) $ IM.fromList bodies'
    case mposis of
        Nothing -> return ()
        Just ps -> modify $ IM.union (fmap (static . Position) ps)

stepIn = do
    -- Tick time.
    t' <- lift $ getCurrentTime
    t  <- get
    put t'
    let dt = realToFrac $ diffUTCTime t' t :: Float
    -- Get the user events and fold them into our InputEnv.
    loadNewEvents
    return dt

play = do


    dt <- stepIn

    -- Possibly reset.
    keys <- ienvKeysDown <$> get
    R.when (S.member Key'R keys) reset

    -- Get the player position from last frame.
    Just (ID player) <- getEntityByName "player"
    mlastPlayerPos <- IM.lookup player <$> getVals

    stepKeyboardControlledThings dt
    stepJoystickControlledThings dt
    -- Get the player position after user input.

    -- Update the player's toon based on velocity
    mnewPlayerPos <- IM.lookup player <$> getVals
    mPlayerToon   <- IM.lookup player <$> getVals

    let mv = do lastPlayerPos <- mlastPlayerPos
                newPlayerPos  <- mnewPlayerPos
                playerToon    <- mPlayerToon
                let v = (unpos newPlayerPos) - (unpos lastPlayerPos)
                    toon = displayPlusVelocity playerToon $ Velocity v
                if v == zero then Nothing
                  else return $ do (ds :: DynamicValue Displayable) <- get
                                   put $ IM.insert player (static toon) ds
                                   lift $ print (playerToon, toon)
    maybe (return ()) id mv

    stepComponents dt
    collidePlayer

    -- Display our game
    displayAll

    -- Clear out the list of input events that happened this frame.
    clearLastEvents

    -- Handle the possibility of quitting.
    handleQuit
    -- Pass some time so we don't hog all the CPU cycles.
    lift $ threadDelay 100

stepComponents dt = do
    (faces :: DynamicValue Displayable)     <- get
    (positions :: DynamicValue Position)  <- get
    (scales :: DynamicValue Scale) <- get
    (rotations :: DynamicValue Rotation) <- get
    (velocities :: DynamicValue Velocity) <- get
    (colors :: DynamicValue Colors) <- get
    (offsets :: DynamicValue PositionOffset) <- get
    put $ stepComponent dt faces
    put $ stepComponent dt positions
    put $ stepComponent dt scales
    put $ stepComponent dt rotations
    put $ stepComponent dt velocities
    put $ stepComponent dt colors
    put $ stepComponent dt offsets

displayAll = do
    (colors     :: Component Colors)         <- getVals
    (positions  :: Component Position)       <- getVals
    (scales     :: Component Scale)          <- getVals
    (rotations  :: Component Rotation)       <- getVals
    (faces      :: Component Displayable)    <- getVals
    (offsets    :: Component PositionOffset) <- getVals

    let positions' = Position <$> (IM.unionWith (^+^) (unposoff <$> offsets)
                                                      (unpos <$> positions))

    window   <- ask >>= lift . getWindow
    renderer <- ask

    let display :: Colors -> Displayable -> Position -> Rotation -> Scale -> IO ()
        display clrs face pos rot scl = (drawWith renderer) clrs
                                                            face
                                                            (unpos pos)
                                                            (unscl scl)
                                                            (unrot rot)

    lift $ do makeContextCurrent $ Just window
              (drawWith renderer) (Colors transparent transparent) CleanFrame zero zero 0
              -- Display all toons.
              let draws = IM.elems $ intersectionWith5 (,,,,) colors faces positions' rotations scales
                  draws' = sortBy (\(_,_,p1,_,_) (_,_,p2,_,_) ->
                                    compare ((unpos p1)^._y) ((unpos p2)^._y))
                                  draws
                  drawIOs = fmap (\(a,b,c,d,e) -> display a b c d e) draws'
              sequence_ drawIOs
              --sequence_ entanglements
              let clrs = Colors transparent $ red `alpha` 0.2
                  scl1 = Scale $ V2 1 1
                  rot0 = Rotation 0

              --mapM_ displayName namesOfToons
              swapBuffers window

loadNewEvents = do
    lift $ pollEvents
    events <- ask >>= lift . getNewEvents
    env    <- get
    let env'  = Prelude.foldl foldInput env events
    put env'

clearLastEvents = modify clearEvents

handleQuit = do
    (wref :: WindowRef) <- ask
    (window :: Window) <- lift $ getWindow wref
    lift $ do shouldClose <- windowShouldClose window
              R.when shouldClose exitSuccess

reset = do
    put (mempty :: DynamicValue Displayable)
    put (mempty :: DynamicValue Position)
    put (mempty :: DynamicValue Scale)
    put (mempty :: DynamicValue Rotation)
    put (mempty :: DynamicValue Velocity)
    put (mempty :: DynamicValue Colors)
    put (mempty :: DynamicValue PositionOffset)

    put (mempty :: Component (PlayerDirection Key))
    put (mempty :: Component (PlayerDirection Int))
    put (mempty :: Component (PlayerRotation Int))
    put (mempty :: M.Map String ID)

    (lift $ getCurrentTime) >>= put
    initialize

data PositionReactive = PositionReactivePosition ID (Dynamic Position)
                      | PositionReactiveScale ID (Dynamic Position)

main :: IO ()
main = do
    wref     <- initWindow (V2 300 600) (V2 600 600) "ld31"
    renderer <- newRenderer =<< getWindow wref
    t        <- getCurrentTime

    runLift $ evalState (mempty :: DynamicValue Displayable)
            $ evalState (mempty :: DynamicValue Position)
            $ evalState (mempty :: DynamicValue Scale)
            $ evalState (mempty :: DynamicValue Rotation)
            $ evalState (mempty :: DynamicValue Velocity)
            $ evalState (mempty :: DynamicValue Colors)
            $ evalState (mempty :: DynamicValue Size)
            $ evalState (mempty :: DynamicValue Mass)
            $ evalState (mempty :: DynamicValue PositionOffset)

            $ evalState (mempty :: Component (PlayerDirection Key))
            $ evalState (mempty :: Component (PlayerDirection Int))
            $ evalState (mempty :: Component (PlayerRotation Int))
            $ evalState (mempty :: Component (M.Map String ID))
            $ evalState (mempty :: M.Map String ID)
            $ evalState emptyInputEnv
            $ evalState t
            $ flip runReader wref
            $ flip runReader renderer
            $ flip runFresh (ID 0) $ do
                initialize
                R.forever play

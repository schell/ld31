{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Rendering where

import Toons
import Gelatin hiding (drawArrays, get, Position)
import Graphics.Rendering.OpenGL hiding (ortho, triangulate, translate, scale, rotate, get, Position)
import qualified Graphics.Rendering.OpenGL as GL
import Shaders
import Data.Maybe
import Data.Typeable
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad hiding (mapM_, sequence_)
import Control.Applicative

type RenderFunc = Colors -> Displayable -> RenderSprite
newtype Renderer = Renderer RenderFunc deriving (Typeable)

drawWith :: Renderer -> RenderFunc
drawWith (Renderer f) = f

data Colors = Colors { foregroundColor :: V4 Float
                     , backgroundColor :: V4 Float
                     } deriving (Typeable, Show)

type RenderSprite = V2 Float -> V2 Float -> Float -> IO ()

data SpriteSheet = SpriteSheet { ssObject :: TextureObject
                               , ssWidth  :: Int
                               , ssHeight :: Int
                               , ssName   :: String
                               }

loadSpriteSheet :: String -> Int -> Int -> IO SpriteSheet
loadSpriteSheet fp w h = do
    t <- fromJust . M.lookup fp <$>
             loadTextureSrc (Relative fp)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    return $ SpriteSheet t w h fp

-- A map of chars to their bitmap position and size in our spritesheet.
charMetrics :: M.Map Char (V2 Int, Int, Int)
charMetrics = M.fromList $ concat $ zipWith mk [row1,row2,row3] [V2 9 1130, V2 9 1138, V2 9 1146]
    where row1    = [' ' .. '@']
          row2    = ['A' .. '`']
          row3    = ['a' .. '~']
          mk :: String -> V2 Int -> [(Char, (V2 Int, Int, Int))]
          mk row p = zip row $ zip3 (length row `pointsFrom` p) (cycle [5]) (cycle [7])
          pointsFrom n p = zipWith (\n' p' -> p' ^+^ V2 (n'*6) 0) [0 .. n] $ cycle [p]

newCharRenderer :: ShaderProgram -> SpriteSheet -> IO (Char -> RenderSprite)
newCharRenderer s ss = do
    let cm = charMetrics
        keys = M.keys cm
        vals = catMaybes $ map (`M.lookup` cm) keys
        kvs = zip keys vals

    draws <- forM kvs $ \(c, (v, w, h)) -> do
        r <- newSpriteRenderer s ss v w h
        return (c,r)

    let m = M.fromList draws
    return $ \c vp vs r -> case M.lookup c m of
                               Nothing -> putStrLn $ c : " has not been loaded."
                               Just f  -> f vp vs r

newTextRenderer :: ShaderProgram -> SpriteSheet -> IO (String -> RenderSprite)
newTextRenderer s ss = do
    cr <- newCharRenderer s ss
    return $ renderText cr

renderText :: (Char -> RenderSprite) -> String -> RenderSprite
renderText render str pos scl rot = renderText' str pos
    where renderText' []       _    = return ()
          renderText' ('\n':str') _ = renderText render str' (pos + scl * V2 0 7) scl rot
          renderText' (c:str') pos' =
              case M.lookup c charMetrics of
                  Nothing             -> renderText' (' ':str') pos'
                  Just (V2 _ _, w, _) -> do render c pos' scl rot
                                            renderText' str' (pos' + scl * V2 (fromIntegral w) 0)

newSpriteRenderer :: ShaderProgram -> SpriteSheet -> V2 Int -> Int -> Int -> IO RenderSprite
newSpriteRenderer s ss p w h = do
    let (hw, hh) = ((fromIntegral w)/2, (fromIntegral h)/2)
        ps = [V2 (-hw) (-hh), V2 hw (-hh), V2 hw hh, V2 (-hw) hh] :: [V2 Float]
        [ssw,ssh] = fmap fromIntegral [ssWidth ss, ssHeight ss]
        [w',h'] = fmap fromIntegral [w, h]
        -- Origin of the rectangle in u,v coords.
        (V2 ox oy) = fmap fromIntegral p & _x %~ (/ ssw) & _y %~ (/ ssh)
        (V2 cx cy) = V2 ox oy ^+^ V2 (w'/ssw) (h'/ssh)
        ts     = [V2 ox oy, V2 cx oy, V2 cx cy, V2 ox cy] :: [V2 Float]
        t      = ssObject ss
    vbov <- bufferVBO s (position2 ps)
    vbot <- bufferVBO s (texcoord ts)

    return $ \vp vs r -> usingShader s $ withTextures2D [t] $ withVBOs s [vbov,vbot] $ do
        let mv = mkM44 $ do translate $ embed vp
                            rotate r (V3 0 0 1)
                            scale $ embedWith vs 1
        updateUniform s $ uniformi "sampler" (0 :: Int)
        updateUniform s $ uniformM4f "modelview" (mv :: M44 Float)
        drawArrays TriangleFan 0 4

newReaperRenderer :: ShaderProgram -> SpriteSheet -> IO (CardinalDirection -> RenderSprite)
newReaperRenderer s ss = do
    south <- newSpriteRenderer s ss (V2 0 1077) 40 51
    east  <- newSpriteRenderer s ss (V2 42 1077) 40 51
    west  <- newSpriteRenderer s ss (V2 83 1077) 40 51
    north <- newSpriteRenderer s ss (V2 125 1077) 40 51

    return $ \d pos scl rot -> case d of
                                   South -> south pos scl rot
                                   East  -> east pos scl rot
                                   West  -> west pos scl rot
                                   North -> north pos scl rot

newCloudRenderer :: ShaderProgram -> SpriteSheet -> IO (Int -> RenderSprite)
newCloudRenderer s ss = do
    one   <- newSpriteRenderer s ss (V2 0 1162) 26 4
    two   <- newSpriteRenderer s ss (V2 0 1167) 26 4
    three <- newSpriteRenderer s ss (V2 0 1172) 26 4
    four  <- newSpriteRenderer s ss (V2 0 1177) 26 4

    return $ \i pos scl rot -> case i of
                                   1 -> one pos scl rot
                                   2 -> two pos scl rot
                                   3 -> three pos scl rot
                                   4 -> four pos scl rot
                                   _ -> return ()

newMoonRenderer :: ShaderProgram -> SpriteSheet -> IO RenderSprite
newMoonRenderer s ss = newSpriteRenderer s ss (V2 135 1028) 40 34

newCobbleStoneRenderer :: ShaderProgram -> SpriteSheet -> IO RenderSprite
newCobbleStoneRenderer s ss = newSpriteRenderer s ss (V2 0 0) 32 32

newBoxRenderer :: ShaderProgram -> IO RenderSprite
newBoxRenderer s = do
    let ps = [V2 (-0.5) (-0.5), V2 0.5 (-0.5), V2 0.5 0.5, V2 (-0.5) 0.5] :: [V2 Float]
        ts = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: [V2 Float]

    vbo <- bufferVBO s (position2 ps)
    tvbo <- bufferVBO s (texcoord ts)

    return $ \vp vs r -> usingShader s $ withVBOs s [vbo, tvbo] $ do
        let mv = mkM44 $ do translate $ embed vp
                            rotate r (V3 0 0 1)
                            scale $ embedWith vs 1
        updateUniform s $ uniformi "sampler" (0 :: Int)
        updateUniform s $ uniformM4f "modelview" (mv :: M44 Float)
        drawArrays TriangleFan 0 4

newFancyBoxRenderer :: ShaderProgram -> SpriteSheet -> IO RenderSprite
newFancyBoxRenderer s ss = do
    tl <- newSpriteRenderer s ss (V2 184 1084) 8 8
    tr <- newSpriteRenderer s ss (V2 193 1084) 8 8
    bl <- newSpriteRenderer s ss (V2 202 1084) 8 8
    br <- newSpriteRenderer s ss (V2 211 1084) 8 8
    l <- newSpriteRenderer s ss (V2 220 1084) 8 8
    r <- newSpriteRenderer s ss (V2 229 1084) 8 8
    t <- newSpriteRenderer s ss (V2 238 1084) 8 8
    b <- newSpriteRenderer s ss (V2 184 1093) 8 8
    box <- newBoxRenderer s
    return $ \pos (V2 hw hh) rot -> do
        let scl = V2 1 1
            topleft = pos ^+^ V2 (-hw) (-hh)
            topright = pos ^+^ V2 hw (-hh)
            botleft = pos ^+^ V2 (-hw) hh
            botright = pos ^+^ V2 hw hh
            startx = topleft^._x + 8
            endx = topright^._x - 8
            starty = topleft^._y + 8
            endy = botleft^._y - 8
            nhs = fromIntegral $ ceiling ((endx - startx) / 8)
            nvs = fromIntegral $ ceiling ((endy - starty) / 8)

        box pos ((botright - 4) - (topleft + 4)) 0

        forM_ (map (\i -> V2 (startx + i*8) (topleft^._y)) [0..nhs]) $ \p -> do
            t p scl rot
        forM_ (map (\i -> V2 (startx + i*8) (botleft^._y)) [0..nhs]) $ \p -> do
            b p scl rot
        forM_ (map (\i -> V2 (topleft^._x) (starty + i*8)) [0..nvs]) $ \p -> do
            l p scl rot
        forM_ (map (\i -> V2 (topright^._x) (starty + i*8)) [0..nvs]) $ \p -> do
            r p scl rot

        tl topleft scl rot
        tr topright scl rot
        bl botleft scl rot
        br botright scl rot


newRenderer :: Window -> IO Renderer
newRenderer window = do
    --s  <- simple2dTextureShader
    s  <- colorReplaceTextureShader
    ss <- loadSpriteSheet "img/oryx_roguelike_16x24.png" 304 1184

    drawFullSheet <- newSpriteRenderer s ss (V2 0 0) 304 1184
    drawReaper    <- newReaperRenderer s ss
    drawText      <- newTextRenderer s ss
    drawBox       <- newBoxRenderer s
    drawCloud     <- newCloudRenderer s ss
    drawMoon      <- newMoonRenderer s ss
    drawCobble    <- newCobbleStoneRenderer s ss
    drawFancyBox  <- newFancyBoxRenderer s ss

    currentProgram $= (Just $ program s)
    clearColor $= toColor4 (black :: V4 Float)
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))

    return $ Renderer $ \(Colors fg bg) thing pos scl rot -> do
        currentProgram $= (Just $ program s)
        updateUniform s $ uniformV4fv "texColor" ([white, black] :: [V4 Float])
        updateUniform s $ uniformV4fv "replaceColor" [fg, bg]

        case thing of
            CleanFrame -> do
                clear [ColorBuffer]
                (w,h) <- getWindowSize window
                let [w',h'] = map fromIntegral [w,h] :: [Float]
                    [w'',h''] = map ((*2) . fromIntegral) [w,h] :: [GLint]

                viewport $= (GL.Position 0 0, Size w'' h'')
                clear [ColorBuffer]
                updateUniform s $ uniformi "sampler" (0 :: Int)
                updateUniform s $ uniformM4f "projection" $ ortho 0 w' 0 h' 0 1

            FullSheet   -> drawFullSheet pos scl rot
            Reaper d    -> drawReaper d pos scl rot
            Text str    -> drawText str pos scl rot
            Box         -> drawBox pos scl rot
            Cloud i     -> drawCloud i pos scl rot
            Moon        -> drawMoon pos scl rot
            CobbleStone -> drawCobble pos scl rot
            FancyBox    -> drawFancyBox pos scl rot
            _           -> return ()

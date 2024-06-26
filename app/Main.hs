{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Apecs
import AppTypes

import Camera
import Circles

import Control.Monad.Reader
import Control.Monad.State (State, state, evalState)

import Data.Aeson
import Data.Bool
import Data.IORef
import Data.List

import Raylib.Core
import Raylib.Core.Audio
import Raylib.Core.Shapes
import Raylib.Core.Text

import Raylib.Util
import Raylib.Util.Colors

import Raylib.Types

import System.Random
import System.Directory
import System.Environment

main :: IO ()
main = do 
    [filename] <- getArgs
    eitherDecodeFileStrict @Simulation filename >>= \case 
        Left  err -> showError err
        Right sim -> runSimulation sim

-- load random music file
getMusicFile :: IO FilePath
getMusicFile = do
    files <- listDirectory "music"
    let musicFiles = filter (isSuffixOf ".mp3") files
    randomIndex <- randomRIO (0, length musicFiles - 1)
    pure $ "music/" ++ musicFiles !! randomIndex

runSimulation :: Simulation -> IO ()
runSimulation sim = do 
    let width  = windowWidth sim 
        height = windowHeight sim

    initAudioDevice

    win   <- initWindow width height (windowTitle sim)
    world <- initWorld

    setConfigFlags [Msaa4xHint]
    setTargetFPS (framerate sim)

    musicFile  <- getMusicFile 
    music <- loadMusicStream musicFile win
    playMusicStream music

    runSystem (mapM_ spawnCircle (entities sim)) world -- spawn initial entities 

    cam    <- newIORef (Camera2D (Vector2 0 0) (Vector2 0 0) 0 1)
    pause  <- newIORef False
    follow <- newIORef Nothing
    musRef <- newIORef music

    gen    <- newStdGen

    let s = AppState { simul        = sim
                     , offsetSpeed  = 11
                     , camera       = cam
                     , paused       = pause
                     , following    = follow
                     , currentMusic = musRef
                     , starGen      = gen
                     }

    whileWindowOpen0 (runApp gameFrame world s) 
    closeWindow win

showError :: String -> IO ()
showError err = do 
    win <- initWindow 800 600 "Error"
    whileWindowOpen0 $ drawing $ do 
        clearBackground black
        drawText err 10 10 20 red
    closeWindow win

gameFrame :: App ()
gameFrame = updateWorld >> liftIO beginDrawing >> renderWorld >> liftIO endDrawing

updateWorld :: App () 
updateWorld = do 
    -- Update music
    music <- asks currentMusic >>= liftIO . readIORef
    liftIO $ updateMusicStream music

    updatePause
    pause <- asks paused >>= liftIO . readIORef

    -- Update framerate
    unless pause $ do 
        plusPressed  <- bool 0 1    <$> liftIO (isKeyDown KeyEqual)
        minusPressed <- bool 0 (-1) <$> liftIO (isKeyDown KeyMinus)

        let dframe = plusPressed + minusPressed

        when (dframe /= 0) $ do
            fps <- liftIO getFPS
            liftIO $ setTargetFPS (fps + dframe)

    updateCamera
    unless pause updateCircles

updatePause :: App () 
updatePause = do 
    pause <- asks paused
    liftIO (isKeyPressed KeySpace) >>= \case 
        False -> pure () 
        True  -> liftIO (modifyIORef pause not)

renderWorld :: App ()
renderWorld = do 
    width  <- asks (windowWidth  . simul)
    height <- asks (windowHeight . simul)
    title  <- asks (windowTitle  . simul)

    cam    <- asks camera    >>= liftIO . readIORef
    follow <- asks following >>= liftIO . readIORef
    pause  <- asks paused    >>= liftIO . readIORef

    -- Render stars
    gen    <- asks starGen

    let starCount = 2000 
        stars     = evalState (replicateM starCount (newStar width height)) gen
        starColor = Color 150 150 150 255

    forM_ stars $ \(pos, size) -> do 
        liftIO $ drawCircleV pos size starColor

    let Vector2 camX camY = camera2D'target cam

    let gridSize  = 50 * camera2D'zoom cam
        gridColor = Color 20 20 20 255
        gridX     = fromIntegral @Int $ round (-camX) `mod` round gridSize
        gridY     = fromIntegral @Int $ round (-camY) `mod` round gridSize

    liftIO $ do 
        clearBackground black

        -- Draw grid
        forM_ [gridX, gridX + gridSize .. fromIntegral width] $ \x -> 
            drawLine (round x) 0 (round x) height gridColor

        forM_ [gridY, gridY + gridSize .. fromIntegral height] $ \y -> 
            drawLine 0 (round y) width (round y) gridColor

    liftIO (beginMode2D cam)
    renderCircles
    liftIO endMode2D

    -- Render simulation title
    liftIO $ drawText (title ++ bool "" " - Paused" pause) 10 10 20 white

    Vector2 mouseX mouseY <- liftIO getMousePosition

    let keybindColor = Color 150 150 150 255

    -- Render keybindings
    if mouseX < 100 && mouseY < 50 + 6 * 30 then liftIO $ do 
        drawText "R    - Reset Camera Position"   10 (50 + 0 * 30) 20 keybindColor
        drawText "Spc - Pause Simulation"         10 (50 + 1 * 30) 20 keybindColor
        drawText "Tab - Follow Body"              10 (50 + 2 * 30) 20 keybindColor
        drawText "Esc - Exit Simulation"          10 (50 + 3 * 30) 20 keybindColor
        drawText "=    - Increase FPS"            10 (50 + 4 * 30) 20 keybindColor
        drawText "-    - Decrease FPS"            10 (50 + 5 * 30) 20 keybindColor
    else liftIO $ do 
        drawText "R"   10 (50 + 0 * 30) 20 keybindColor
        drawText "Spc" 10 (50 + 1 * 30) 20 keybindColor
        drawText "Tab" 10 (50 + 2 * 30) 20 keybindColor
        drawText "Esc" 10 (50 + 3 * 30) 20 keybindColor
        drawText "="   10 (50 + 4 * 30) 20 keybindColor
        drawText "-"   10 (50 + 5 * 30) 20 keybindColor

    -- Render entity name currently following
    case follow of 
        Nothing -> pure ()
        Just e  -> do 
            Title t                  <- lift (get e)
            CircleColor c            <- lift (get e)
            Position (Vector2 x y)   <- lift (get e)
            Velocity (Vector2 dx dy) <- lift (get e)

            liftIO $ do 
                drawText t 10 (height - 140) 20 c

                drawText ("x: " ++ show @Int (round x)) 10 (height - 105) 20 c 
                drawText ("y: " ++ show @Int (round y)) 10 (height - 80) 20 c
    
                drawText ("dx: " ++ show @Int (round dx)) 10 (height - 55) 20 c
                drawText ("dy: " ++ show @Int (round dy)) 10 (height - 30) 20 c

newStar :: Int -> Int -> State StdGen (Vector2, Float)
newStar width height = do 
    x    <- state $ randomR (0, fromIntegral width)
    y    <- state $ randomR (0, fromIntegral height)
    size <- state $ randomR (1, 2)

    return (Vector2 x y, size)

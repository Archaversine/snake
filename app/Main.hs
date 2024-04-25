{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Apecs
import AppTypes

import Camera
import Circles

import Control.Monad.Reader

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

    let s = AppState { simul        = sim
                     , offsetSpeed  = 11
                     , camera       = cam
                     , paused       = pause
                     , following    = follow
                     , currentMusic = musRef
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

    -- Render entity name currently following
    case follow of 
        Nothing -> pure ()
        Just e  -> do 
            Title t                  <- lift (get e)
            CircleColor c            <- lift (get e)
            Position (Vector2 x y)   <- lift (get e)
            Velocity (Vector2 dx dy) <- lift (get e)

            let posText = "(x: " ++ show @Int (round x) ++ ", y: " ++ show @Int (round y) ++ ")"
                velText = "(dx: " ++ show @Int (round dx) ++ ", dy: " ++ show @Int (round dy) ++ ")"
                label = t ++ " - " ++ posText ++ " - " ++ velText

            liftIO $ drawText label 10 (height - 30) 20 c


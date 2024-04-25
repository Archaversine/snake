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
import Data.IORef

import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text

import Raylib.Util
import Raylib.Util.Colors

import Raylib.Types

import System.Environment

main :: IO ()
main = do 
    [filename] <- getArgs
    eitherDecodeFileStrict @Simulation filename >>= \case 
        Left  err -> showError err
        Right sim -> runSimulation sim

runSimulation :: Simulation -> IO ()
runSimulation sim = do 
    let width  = windowWidth sim 
        height = windowHeight sim

    win   <- initWindow width height (windowTitle sim)
    world <- initWorld

    setConfigFlags [Msaa4xHint]
    setTargetFPS (framerate sim)
    runSystem (mapM_ spawnCircle (entities sim)) world -- spawn initial entities 

    cam <- newIORef (Camera2D (Vector2 0 0) (Vector2 0 0) 0 1)

    let s = AppState { simul       = sim
                     , offsetSpeed = 11
                     , camera      = cam
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
updateWorld = updateCamera >> updateCircles

renderWorld :: App ()
renderWorld = do 
    width  <- asks (windowWidth . simul)
    height <- asks (windowHeight . simul)
    cam    <- asks camera >>= liftIO . readIORef

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



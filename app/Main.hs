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

    setTargetFPS (framerate sim)
    runSystem (mapM_ spawnCircle (entities sim)) world -- spawn initial entities 

    cam <- newIORef (Camera2D (Vector2 0 0) (Vector2 0 0) 0 1)

    let s = AppState { simul       = sim
                     , offsetSpeed = 10
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

    let Vector2 camX camY = camera2D'offset cam

    let gridSize  = 10 
        gridColor = Color 20 20 20 255
        gridX     = round camX `mod` gridSize
        gridY     = round camY `mod` gridSize

    liftIO $ do 
        clearBackground black

        -- Draw grid
        forM_ [gridX, gridX + gridSize .. width] $ \x -> 
            drawLine x 0 x height gridColor

        forM_ [gridY, gridY + gridSize .. height] $ \y -> 
            drawLine 0 y width y gridColor

    liftIO (beginMode2D cam)
    renderCircles
    liftIO endMode2D



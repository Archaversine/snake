module Camera where

import AppTypes

import Control.Monad.Reader

import Data.Bool
import Data.IORef

import Raylib.Core
import Raylib.Types
import Raylib.Util.Math

updateCamera :: App () 
updateCamera = updateKeyboardPan >> checkResetKey >> updateMousePan

updateKeyboardPan :: App () 
updateKeyboardPan = do 
    cam   <- asks camera
    speed <- asks offsetSpeed

    left  <- liftIO $ (||) <$> isKeyDown KeyA <*> isKeyDown KeyLeft
    right <- liftIO $ (||) <$> isKeyDown KeyD <*> isKeyDown KeyRight 
    up    <- liftIO $ (||) <$> isKeyDown KeyW <*> isKeyDown KeyUp 
    down  <- liftIO $ (||) <$> isKeyDown KeyS <*> isKeyDown KeyDown

    let dx = bool 0 1 right - bool 0 1 left
        dy = bool 0 1 down  - bool 0 1 up
        v  = Vector2 (dx * speed) (dy * speed)

    liftIO $ modifyIORef cam (\c -> c { camera2D'offset = camera2D'offset c |-| v })
    
checkResetKey :: App () 
checkResetKey = do 
    cam   <- asks camera
    reset <- liftIO (isKeyPressed KeyR)

    when reset $ liftIO $ do 
        modifyIORef cam (\c -> c { camera2D'offset = Vector2 0 0 })

updateMousePan :: App ()
updateMousePan = do 
    cam        <- asks camera
    rightMouse <- liftIO (isMouseButtonDown MouseButtonRight)

    when rightMouse $ liftIO $ do 
        mouseV <- getMouseDelta
        modifyIORef cam (\c -> c { camera2D'offset = camera2D'offset c |+| mouseV })

module Camera where

import AppTypes

import Control.Monad.Reader

import Data.Bool
import Data.IORef

import Raylib.Core
import Raylib.Types
import Raylib.Util.Math

updateCamera :: App () 
updateCamera = updateKeyboardPan 
            >> checkResetKey 
            >> updateMousePan 
            >> updateZoom 

updateKeyboardPan :: App () 
updateKeyboardPan = do 
    cam   <- asks camera
    speed <- asks offsetSpeed

    left  <- liftIO $ (||) <$> isKeyDown KeyA <*> isKeyDown KeyLeft
    right <- liftIO $ (||) <$> isKeyDown KeyD <*> isKeyDown KeyRight 
    up    <- liftIO $ (||) <$> isKeyDown KeyW <*> isKeyDown KeyUp 
    down  <- liftIO $ (||) <$> isKeyDown KeyS <*> isKeyDown KeyDown

    let dx  = bool 0 1 right - bool 0 1 left
        dy  = bool 0 1 down  - bool 0 1 up
        vec = Vector2 (dx * speed) (dy * speed)

    moveCamera cam (vec |* negate 1)
    
checkResetKey :: App () 
checkResetKey = do 
    cam   <- asks camera
    reset <- liftIO (isKeyPressed KeyR)

    when reset $ liftIO $ do 
        modifyIORef cam (\c -> c { camera2D'offset = Vector2 0 0, camera2D'target = Vector2 0 0, camera2D'zoom = 1 })

updateMousePan :: App ()
updateMousePan = do 
    cam        <- asks camera
    rightMouse <- liftIO (isMouseButtonDown MouseButtonRight)

    when rightMouse $ liftIO $ do 
        mouseV <- getMouseDelta
        moveCamera cam mouseV

updateZoom :: App () 
updateZoom = do 
    cam    <- asks camera
    wheel  <- liftIO getMouseWheelMove

    let zoomIncrement = 0.125
        zoom          = wheel * zoomIncrement

    when (wheel /= 0) $ liftIO $ do
        camObj         <- readIORef cam
        mouseScreenPos <- getMousePosition
        mouseWorldPos  <- getScreenToWorld2D mouseScreenPos camObj

        modifyIORef cam (\c -> c { camera2D'offset = mouseScreenPos
                                 , camera2D'target = mouseWorldPos
                                 , camera2D'zoom   = camera2D'zoom c + zoom
                                 })

        constrainZoom cam zoomIncrement

moveCamera :: MonadIO m => IORef Camera2D -> Vector2 -> m ()
moveCamera camRef vel = liftIO $ do 
    cam <- readIORef camRef
    let v = vel |* (-1 / camera2D'zoom cam)
    writeIORef camRef (cam { camera2D'target = camera2D'target cam |+| v })

constrainZoom :: MonadIO m => IORef Camera2D -> Float -> m ()
constrainZoom camRef minZoom = liftIO $ do 
    cam <- readIORef camRef 
    let zoom = max minZoom (camera2D'zoom cam) 
    writeIORef camRef (cam { camera2D'zoom = zoom })

module Camera where

import AppTypes

import Control.Monad.Reader

import Data.Bool
import Data.IORef

import Raylib.Core
import Raylib.Types

updateCamera :: App () 
updateCamera = updateKeyboardPan >> checkResetKey >> updateMousePan

updateKeyboardPan :: App () 
updateKeyboardPan = do 
    xOff  <- asks xOffset
    yOff  <- asks yOffset
    speed <- asks offsetSpeed

    left  <- liftIO $ (||) <$> isKeyDown KeyA <*> isKeyDown KeyLeft
    right <- liftIO $ (||) <$> isKeyDown KeyD <*> isKeyDown KeyRight 
    up    <- liftIO $ (||) <$> isKeyDown KeyW <*> isKeyDown KeyUp 
    down  <- liftIO $ (||) <$> isKeyDown KeyS <*> isKeyDown KeyDown

    let dx = bool 0 1 right - bool 0 1 left
        dy = bool 0 1 down  - bool 0 1 up

    liftIO $ modifyIORef xOff (subtract (dx * speed))
    liftIO $ modifyIORef yOff (subtract (dy * speed))
    
checkResetKey :: App () 
checkResetKey = do 
    xOff <- asks xOffset
    yOff <- asks yOffset

    reset <- liftIO (isKeyPressed KeyR)

    when reset $ liftIO $ do 
        writeIORef xOff 0
        writeIORef yOff 0

updateMousePan :: App ()
updateMousePan = do 
    xOff <- asks xOffset
    yOff <- asks yOffset

    rightMouse <- liftIO (isMouseButtonDown MouseButtonRight)

    when rightMouse $ liftIO $ do 
        Vector2 mouseDX mouseDY <- getMouseDelta
        modifyIORef xOff (+mouseDX)
        modifyIORef yOff (+mouseDY)

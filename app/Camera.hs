{-# LANGUAGE LambdaCase #-}

module Camera where

import AppTypes

import Apecs
import Apecs.Util

import Control.Monad.Reader

import Data.Bool
import Data.IORef
import Data.Monoid

import Raylib.Core
import Raylib.Types
import Raylib.Util.Math

updateCamera :: App () 
updateCamera = updateKeyboardPan 
            >> checkResetKey 
            >> updateMousePan 
            >> updateZoom 
            >> updateFollow

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
    cam    <- asks camera
    reset  <- liftIO (isKeyPressed KeyR)
    follow <- asks following

    when reset $ liftIO $ do 
        writeIORef follow Nothing
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
    follow <- asks following

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
        writeIORef follow Nothing -- Reset follow if zoom in or out

updateFollowTabKey :: App () 
updateFollowTabKey = do 
    tab       <- liftIO (isKeyPressed KeyTab)
    followRef <- asks following
    follow    <- liftIO (readIORef followRef)
    counter   <- getSum . getCounter <$> lift (get global)

    when tab $ liftIO $ case follow of 
        Nothing -> when (counter > 0) $ do 
            writeIORef followRef (Just (Entity 0))
        Just (Entity i) -> writeIORef followRef next 
            where next = if i + 1 < counter then Just (Entity (i + 1)) else Nothing

updateFollow :: App () 
updateFollow = do 
    updateFollowTabKey

    width  <- asks (windowWidth . simul)
    height <- asks (windowHeight . simul)

    let offset = Vector2 (fromIntegral width / 2) (fromIntegral height / 2)

    (asks following >>= liftIO . readIORef) >>= \case 
        Nothing -> pure ()
        Just e  -> do 
            Position pos <- lift (get e)
            cam          <- asks camera

            liftIO $ modifyIORef cam $ \c -> c { camera2D'target = pos |-| offset 
                                               , camera2D'offset = Vector2 0 0
                                               , camera2D'zoom = 1 
                                               }

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

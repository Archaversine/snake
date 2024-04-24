{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Apecs

import Control.Monad.Reader

import Data.Aeson
import Data.Bool (bool)
import Data.List (foldl', find)
import Data.IORef

import GHC.Generics (Generic)

import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text

import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math

import Raylib.Types

import System.Environment

newtype Position    = Position Vector2
newtype Velocity    = Velocity Vector2
newtype Size        = Size Float
newtype Mass        = Mass Float
newtype EntityID    = EntityID Entity
newtype CircleColor = CircleColor Color
newtype Title       = Title String
newtype Trail       = Trail [Vector2]

data Immovable = Immovable

instance FromJSON Position where 
    parseJSON = withObject "position" $ \v -> Position 
        <$> (Vector2 <$> v .: "x" <*> v .: "y")

instance FromJSON Velocity where 
    parseJSON = withObject "velocity" $ \v -> Velocity 
        <$> (Vector2 <$> v .: "x" <*> v .: "y")

instance FromJSON CircleColor where 
    parseJSON = withObject "color" $ \v -> CircleColor 
        <$> (Color <$> v .: "r" <*> v .: "g" <*> v .: "b" <*> v .: "a")

makeWorldAndComponents "World" [ ''Position
                               , ''Velocity
                               , ''Size
                               , ''Mass
                               , ''EntityID
                               , ''Immovable
                               , ''CircleColor
                               , ''Title
                               , ''Trail
                               ]

data EntityData = EntityData { entityTitle :: Title 
                             , entityPos   :: Position
                             , entityVel   :: Velocity
                             , entityMass  :: Mass
                             , entitySize  :: Size
                             , immovable   :: Bool
                             , entityColor :: CircleColor
                             }

instance FromJSON EntityData where 
    parseJSON = withObject "entity" $ \v -> EntityData 
        <$> (Title <$> v .: "title")
        <*> v .: "position"
        <*> v .: "velocity"
        <*> (Mass <$> v .: "mass")
        <*> (Size <$> v .: "size")
        <*> v .:? "immovable" .!= False
        <*> v .: "color"

data Simulation = Simulation { entities       :: [EntityData] 
                             , windowWidth    :: Int 
                             , windowHeight   :: Int 
                             , windowTitle    :: String
                             , framerate      :: Int
                             , maxTrailLength :: Int
                             } deriving Generic

instance FromJSON Simulation

data AppState = AppState { simul       :: Simulation 
                         , xOffset     :: IORef Float
                         , yOffset     :: IORef Float
                         , offsetSpeed :: IORef Float
                         }

type App = ReaderT AppState (SystemT World IO)

runApp :: App a -> World -> AppState -> IO a
runApp app world initial = runSystem (runReaderT app initial) world

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

    s <- AppState sim <$> newIORef 0 <*> newIORef 0 <*> newIORef 3

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
updateWorld = updateCamera >> updateEntities

updateCamera :: App () 
updateCamera = do 
    xOff <- asks xOffset
    yOff <- asks yOffset
    s    <- asks offsetSpeed

    speed <- liftIO (readIORef s)

    left  <- liftIO $ (||) <$> isKeyDown KeyA <*> isKeyDown KeyLeft
    right <- liftIO $ (||) <$> isKeyDown KeyD <*> isKeyDown KeyRight 
    up    <- liftIO $ (||) <$> isKeyDown KeyW <*> isKeyDown KeyUp 
    down  <- liftIO $ (||) <$> isKeyDown KeyS <*> isKeyDown KeyDown

    let dx = bool 0 1 right - bool 0 1 left
        dy = bool 0 1 down  - bool 0 1 up

    liftIO $ modifyIORef xOff (subtract (dx * speed))
    liftIO $ modifyIORef yOff (subtract (dy * speed))

    reset <- liftIO (isKeyPressed KeyR)

    when reset $ liftIO $ do 
        writeIORef xOff 0
        writeIORef yOff 0

updateEntities :: App ()
updateEntities = do 
    lift $ cmapM_ $ \(Position p1, Velocity v, Mass m1, EntityID entity1, _ :: Not Immovable) -> do 
        vecs <- collect $ \(Position p2, Mass m2, EntityID entity2) -> do 
            if entity1 /= entity2 then do 
                let dist  = vectorDistance p1 p2
                    force = (m1 * m2) / (dist * dist)
                    acc   = force / m1
                    vec   = p2 |-| p1
                    grav  = vectorNormalize vec |* acc
                Just grav
            else Nothing

        let vel = foldl' (|+|) (Vector2 0 0) vecs
        set entity1 (Velocity (vel |+| v))

    -- Apply velocity to positions  
    -- Negate velocity if touching another entity (collision detection)
    lift $ cmapM_ $ \(Position currentPos, Velocity v, Size s1, EntityID entity1, _ :: Not Immovable) -> do 
        let nextPos = currentPos |+| v

        rest <- collect $ \(Position p2, Size s2, EntityID entity2) -> do 
            if entity1 == entity2 then Nothing else Just (p2, s2)

        case find (circleCollision (nextPos, s1)) rest of
            Nothing -> set entity1 (Position nextPos)
            Just (p2, _) -> do 
                let mag = magnitude v
                    vec = vectorNormalize (p2 |-| currentPos) |* mag
                    rot = v |+| vector2Rotate vec pi -- rotate 180
                set entity1 (Velocity rot, Position (currentPos |+| rot))

    -- Update trails
    maxLength <- asks (maxTrailLength . simul)
    lift $ cmap $ \(Position p, Trail trail, _ :: Not Immovable) -> Trail (p : take (maxLength - 1) trail)

circleCollision :: (Vector2, Float) -> (Vector2, Float) -> Bool
circleCollision (p1, r1) (p2, r2) = vectorDistance p1 p2 < r1 + r2

renderWorld :: App ()
renderWorld = do 
    width  <- asks (windowWidth . simul)
    height <- asks (windowHeight . simul)

    camX <- asks xOffset >>= liftIO . readIORef
    camY <- asks yOffset >>= liftIO . readIORef

    let gridSize = 10 
        gridX    = round camX `mod` gridSize
        gridY    = round camY `mod` gridSize

    liftIO $ do 
        clearBackground black

        -- Draw grid
        forM_ [gridX, gridX + gridSize .. width] $ \x -> 
            drawLine x 0 x height (Color 10 10 10 255)

        forM_ [gridY, gridY + gridSize .. height] $ \y -> 
            drawLine 0 y width y (Color 10 10 10 255)

    renderEntities

renderEntities :: App ()
renderEntities = do 
    xOff <- asks xOffset
    yOff <- asks yOffset

    camX <- liftIO (readIORef xOff)
    camY <- liftIO (readIORef yOff)

    let cam = Vector2 camX camY

    lift $ cmapM_ $ \(Position (Vector2 x y), Size size, Mass m, CircleColor color, Title title, Trail trail) -> liftIO $ do
        let x' = x + camX
            y' = y + camY

        renderTrail (map (|+| cam) trail) color
        drawCircle (round x') (round y') size color
        drawText title (round x') (round $ y' - size - 15) 10 white
        drawText (show m) (round x') (round $ y' + size + 5) 10 white

renderTrail :: MonadIO m => [Vector2] -> Color -> m ()
renderTrail (Vector2 x1 y1 : ts@(Vector2 x2 y2 : _)) color = do 
    liftIO $ drawLine (round x1) (round y1) (round x2) (round y2) color
    renderTrail ts color
renderTrail _ _ = return ()

spawnCircle :: EntityData -> System World ()
spawnCircle entity = do 
    e <- newEntity ( entityPos entity
                   , entityVel entity
                   , entitySize entity
                   , entityMass entity
                   , entityColor entity
                   , entityTitle entity
                   , Trail []
                   )
    when (immovable entity) (set e Immovable)
    set e (EntityID e)


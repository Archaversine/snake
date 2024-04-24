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

import Control.Monad (when, forM_)

import Data.Aeson
import Data.List (foldl', find)

import GHC.Generics (Generic)

import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text

import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math

import Raylib.Types

import System.Environment

windowDims :: Num a => (a, a)
windowDims = (800, 600)

newtype Position    = Position Vector2
newtype Velocity    = Velocity Vector2
newtype Size        = Size Float
newtype Mass        = Mass Float
newtype EntityID    = EntityID Entity
newtype CircleColor = CircleColor Color
newtype Title       = Title String

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

makeWorldAndComponents "World" [''Position, ''Velocity, ''Size, ''Mass, ''EntityID, ''Immovable, ''CircleColor, ''Title]

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

data Simulation = Simulation { entities     :: [EntityData] 
                             , windowWidth  :: Int 
                             , windowHeight :: Int 
                             , windowTitle  :: String
                             , framerate    :: Int
                             } deriving Generic

instance FromJSON Simulation

gravityConstant :: Float
gravityConstant = 1

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

    whileWindowOpen0 (runSystem (gameFrame width height) world) 
    closeWindow win

showError :: String -> IO ()
showError err = do 
    win <- initWindow 800 600 "Error"
    whileWindowOpen0 $ drawing $ do 
        clearBackground black
        drawText err 10 10 20 red
    closeWindow win

gameFrame :: Int -> Int -> System World ()
gameFrame width height = updateEntities >> liftIO beginDrawing >> renderWorld width height >> liftIO endDrawing

updateEntities :: System World ()
updateEntities = do 
    delta <- liftIO getFrameTime

    cmapM_ $ \(Position p1, Velocity v, Mass m1, EntityID entity1, _ :: Not Immovable) -> do 
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
    cmapM_ $ \(Position currentPos, Velocity v, Size s1, EntityID entity1, _ :: Not Immovable) -> do 
        let nextPos = currentPos |+| (v |* delta)

        rest <- collect $ \(Position p2, Size s2, EntityID entity2) -> do 
            if entity1 == entity2 then Nothing else Just (p2, s2)

        case find (circleCollision (nextPos, s1)) rest of
            Nothing -> set entity1 (Position nextPos)
            Just (p2, _) -> do 
                let mag = magnitude v
                    vec = vectorNormalize (p2 |-| currentPos) |* mag
                    rot = v |+| vector2Rotate vec pi -- rotate 180
                set entity1 (Velocity rot, Position (currentPos |+| (rot |* delta)))

circleCollision :: (Vector2, Float) -> (Vector2, Float) -> Bool
circleCollision (p1, r1) (p2, r2) = vectorDistance p1 p2 < r1 + r2

renderWorld :: Int -> Int -> System World ()
renderWorld width height = do 
    liftIO $ do 
        clearBackground black

        -- Draw grid
        forM_ [0, 10 .. width] $ \x -> 
            drawLine x 0 x height (Color 10 10 10 255)

        forM_ [0, 10 .. height] $ \y -> 
            drawLine 0 y width y (Color 10 10 10 255)

    renderEntities

renderEntities :: System World ()
renderEntities = cmapM_ $ \(Position (Vector2 x y), Size size, Mass m, CircleColor color, Title title) -> liftIO $ do
        drawCircle (round x) (round y) size color
        drawText title (round x) (round $ y - size - 15) 10 white
        drawText (show m) (round x) (round $ y + size + 5) 10 white

spawnCircle :: EntityData -> System World ()
spawnCircle entity = do 
    e <- newEntity (entityPos entity, entityVel entity, entitySize entity, entityMass entity, entityColor entity, entityTitle entity)
    when (immovable entity) (set e Immovable)
    set e (EntityID e)


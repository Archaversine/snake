{-# LANGUAGE ScopedTypeVariables #-}

module Circles where

import Apecs
import AppTypes

import Control.Monad.Reader

import Data.List (foldl', find)
import Data.IORef

import Raylib.Core.Shapes
import Raylib.Core.Text

import Raylib.Types

import Raylib.Util.Colors
import Raylib.Util.Math

import Trails

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

updateCircles :: App ()
updateCircles = do 
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

renderCircles :: App ()
renderCircles = do 
    -- Render Entity trails underneath everything else
    lift $ cmapM_ $ \(CircleColor color, Trail trail) -> liftIO $ do 
        renderTrail trail color

    lift $ cmapM_ $ \(Position (Vector2 x y), Size size, Mass m, CircleColor color, Title title) -> liftIO $ do
        drawCircle (round x) (round y) size color
        drawText title (round x) (round $ y - size - 15) 10 white
        drawText (show m) (round x) (round $ y + size + 5) 10 white

circleCollision :: (Vector2, Float) -> (Vector2, Float) -> Bool
circleCollision (p1, r1) (p2, r2) = vectorDistance p1 p2 < r1 + r2

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AppTypes where

import Apecs

import Control.Monad.Reader

import Data.Aeson
import Data.IORef

import GHC.Generics (Generic)

import Raylib.Types

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


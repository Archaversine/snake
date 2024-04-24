module Trails where

import Control.Monad.Reader

import Raylib.Core.Shapes

import Raylib.Types

renderTrail :: MonadIO m => [Vector2] -> Color -> m ()
renderTrail (Vector2 x1 y1 : ts@(Vector2 x2 y2 : _)) color = do 
    liftIO $ drawLine (round x1) (round y1) (round x2) (round y2) color
    renderTrail ts color
renderTrail _ _ = return ()


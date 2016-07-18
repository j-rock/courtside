{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Courtside.GameState
    ( module Game.Courtside.GameState.Court
    , module Game.Courtside.GameState.Athlete
    , GameState(..)
    , gsCamera
    , gsCourt
    , gsAthlete
    , gsShouldExit
    ) where

import Game.Lib
import Game.Courtside.GameState.Court
import Game.Courtside.GameState.Athlete

data GameState = GameState {
                   _gsCamera     :: Camera
                 , _gsCourt      :: Court
                 , _gsAthlete    :: Athlete
                 , _gsShouldExit :: Bool
                 }

makeLenses ''GameState

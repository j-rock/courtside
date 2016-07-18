{-# LANGUAGE TemplateHaskell #-}

module Game.Courtside.GameState.Athlete where

import Game.Lib

data AthleteMotionState = Walking | Running | Jumping
                         deriving (Eq)

data AthleteMotion = AthleteMotion {
                      _amState    :: AthleteMotionState
                    , _amPosition :: Position
                    , _amVelocity :: Velocity
                    }

data Athlete = Athlete {
              _athHeight :: Double
            , _athWidth  :: Double
            , _athMotion :: AthleteMotion
            }

makeAllLenses [ ''AthleteMotion
              , ''Athlete
              ]

defaultAthlete :: Athlete
defaultAthlete =
    Athlete {
      _athHeight = 5
    , _athWidth = 5
    , _athMotion = AthleteMotion {
        _amState    = Walking
      , _amPosition = Position 0
      , _amVelocity = Velocity $ V3 44 0 2.2
      }
    }

athPosition :: Lens' Athlete Position
athPosition = athMotion . amPosition

athVelocity :: Lens' Athlete Velocity
athVelocity = athMotion . amVelocity

athState :: Lens' Athlete AthleteMotionState
athState = athMotion . amState

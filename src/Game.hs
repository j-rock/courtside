{-# LANGUAGE OverloadedStrings #-}

module Game
  ( module Game.Courtside
  , module Game.Lib
  , module Game.SFML
  , runGame
  ) where

import Game.Courtside
import Game.Lib
import Game.SFML

import qualified Game.SFML as SFML
import qualified Game.Courtside as Courtside


runGame :: IO ()
runGame = SFML.runSFML $ do
              let defaultResolution = ScreenResolution 1536 846
              ctx <- SFML.newContext "Courtside" defaultResolution
              initialState <- Courtside.initialize ctx
              SFML.loop ctx initialState

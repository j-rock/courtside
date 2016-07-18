{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Courtside.GameState.Court where

import Game.Lib

data Court = Court {
               _crtHeight :: Double
             , _crtWidth  :: Double
             , _crtDepth  :: Double
             } deriving (Show)

makeLenses ''Court

defaultCourt :: Court
defaultCourt =
    Court {
      _crtHeight = 29.7
    , _crtWidth  = 177.9
    , _crtDepth  = 2.1
    }

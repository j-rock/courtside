{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.SFML.Context where

import Data.String (IsString)

import Control.Monad.SFML (SFML)
import qualified Control.Monad.SFML.Graphics as SFML
import qualified Control.Monad.SFML.System as SFML

import qualified SFML.Window as SF
import qualified SFML.Graphics as SF

import Game.Lib.Lens
import Game.Lib.Dimensions


data Context = Context {
                 _ctxWindow :: SF.RenderWindow
               , _ctxClock  :: SF.Clock
               }

makeLenses ''Context

newContext :: WindowTitle -> ScreenResolution -> SFML Context
newContext (WindowTitle str) (ScreenResolution (Pixels width) (Pixels height)) =
    let sfCtxSettings  = Just $ SF.ContextSettings 24 8 0 1 2
        videoMode      = SF.VideoMode width height 32
    in do window <- SFML.createRenderWindow videoMode str [SF.SFDefaultStyle] sfCtxSettings
          SFML.setFramerateLimit window 60
          clock  <- SFML.createClock
          return $ Context window clock

newtype WindowTitle = WindowTitle {
                        _wtText :: String
                      } deriving (IsString)

getContextScreenResolution :: Context -> SFML ScreenResolution
getContextScreenResolution c =
    do SF.Vec2u x y <- SFML.getWindowSize $ c^.ctxWindow
       let conv = Pixels . fromIntegral
       return $ ScreenResolution (conv x) (conv y)

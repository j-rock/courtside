{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.SFML.Loop where

import Control.Monad.SFML (SFML)
import qualified Control.Monad.SFML.Graphics as SFML
import qualified Control.Monad.SFML.System as SFML
import qualified SFML.Graphics as SF
import qualified SFML.System as SF
import qualified SFML.Window as SF

import Game.SFML.Context
import Game.Lib.Lens

data LoopState s = LoopState {
                     _loopRender :: s -> SF.RenderWindow -> SFML ()
                   , _loopUpdate :: s -> SF.Time -> [SF.SFEvent] -> SFML (Maybe s)
                   , _loopState  :: s
                   }

makeLenses ''LoopState


loop :: Context -> LoopState s -> SFML ()
loop ctx@Context{..} l@LoopState{..} =
    do -- Event handling
       evts <- pollEvents _ctxWindow
       if any isCloseEvt evts
       then return ()
       else do -- Render phase
            SFML.clearRenderWindow _ctxWindow $ SF.Color 0 0 0 255
            _loopRender _loopState _ctxWindow
            SFML.display _ctxWindow

            -- Update Phase
            dt <- SFML.restartClock  _ctxClock
            maybeState <- _loopUpdate _loopState dt evts
            case maybeState of
                Nothing    -> return ()
                Just state -> loop ctx (l & loopState .~ state)


pollEvents :: SF.RenderWindow -> SFML [SF.SFEvent]
pollEvents window = untilNothing (SFML.pollEvent window)
  where untilNothing :: Monad m => m (Maybe a) -> m [a]
        untilNothing acc = go []
          where go as = do m <- acc
                           case m of
                               Nothing -> return as
                               Just a  -> go (a:as)

isCloseEvt :: SF.SFEvent -> Bool
isCloseEvt SF.SFEvtClosed = True
isCloseEvt _              = False

{-
data SFEvent = SFEvtClosed
             | SFEvtResized
             | SFEvtLostFocus
             | SFEvtGainedFocus
             | SFEvtTextEntered
             | SFEvtKeyPressed
             | SFEvtKeyReleased
             | SFEvtMouseWheelMoved
             | SFEvtMouseWheelScrolled
             | SFEvtMouseButtonPressed
             | SFEvtMouseButtonReleased
             | SFEvtMouseMoved
             | SFEvtMouseEntered
             | SFEvtMouseLeft
             | SFEvtJoystickButtonPressed
             | SFEvtJoystickButtonReleased
             | SFEvtJoystickMoved
             | SFEvtJoystickConnected
             | SFEvtJoystickDisconnected
 -}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Game.Courtside.Update where

import Game.SFML (SFML)
import qualified Game.SFML as SFML
import qualified SFML.Window as SF

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State

import Game.Courtside.GameState
import Game.Lib


type StateUpdate a = StateT GameState SFML a

update :: GameState -> SF.Time -> [SF.SFEvent] -> SFML (Maybe GameState)
update m dt _ =
    do m' <- updateStateT m $ do
                 boolM exitEvent setExit $ do
                     tryMoveAthlete dt
                     updateCamera
       decideToExit m'


updateStateT :: Monad m => s -> StateT s m a -> m s
updateStateT = flip State.execStateT

decideToExit :: GameState -> SFML (Maybe GameState)
decideToExit m = pure $ bool (m^.gsShouldExit) Nothing (Just m)

setExit :: StateUpdate ()
setExit = gsShouldExit .= True

exitEvent :: StateUpdate Bool
exitEvent = areAnyKeysPressed [SF.KeyEscape, SF.KeyQ]

isKeyPressed :: SF.KeyCode -> StateUpdate Bool
isKeyPressed = State.lift . SFML.isKeyPressed

areAnyKeysPressed :: [SF.KeyCode] -> StateUpdate Bool
areAnyKeysPressed = anyOfM isKeyPressed


tryMoveAthlete :: SF.Time -> StateUpdate ()
tryMoveAthlete dt =
    let dtFactor = fromRational . toRational $ SF.asSeconds dt

        dirKeys = [SF.KeyW, SF.KeyS, SF.KeyA, SF.KeyD]
        dirs    = [V3 0 0 (-1), V3 0 0 1, V3 (-1) 0 0, V3 1 0 0]

        matchDir key d = boolM (isKeyPressed key) (return d) (return $ V3 0 0 0)

        dirCos (V3 0 _ z) = V3 0 0 z
        dirCos (V3 x _ z) = let theta    = atan (z / x)
                                (st, ct) = (sin theta, cos theta)
                            in if| x < 0     -> V3 (negate ct) 0 (negate st)
                                 | otherwise -> V3 ct          0 st

    in do gs <- State.get
          direction <- sum <$> zipWithM matchDir dirKeys dirs

          zoom gsAthlete $
              athPosition.pos %= (+) (dtFactor .* dirCos direction * gs^.gsAthlete.athVelocity.vel)

          gs' <- State.get
          -- -- Keep athlete in bounds
          zoom gsAthlete $
              athPosition .= boundedAthlete (gs'^.gsCourt) (gs'^.gsAthlete)


-- Bear in mind, the Athlete's position
-- is his bottom-left corner
boundedAthlete :: Court -> Athlete -> Position
boundedAthlete Court{..} a@Athlete{..} =
    let V3 x y z = a^.athPosition.pos

        x' = max 0 $ min (_crtWidth-_athWidth) x
        y' = max 0 $ min _crtHeight y
        z' = min 0 $ max (negate _crtDepth) z

    in Position (V3 x' y' z')

updateCamera :: StateUpdate ()
updateCamera =
    let on key acc =
            whenM (isKeyPressed key) $
                do _ <- zoom gsCamera acc
                   return ()

        v  = 1

    in do on SF.KeyLeft $ camPosition.pos %= (+) (V3 (-v) 0    0)
          on SF.KeyRight $ camPosition.pos %= (+) (V3 v    0    0)

          -- Keep Camera in bounds
          zoom gsCamera $
              -- constants determined experimentally
              camPosition.pos.v3x %= min 147 . max 31

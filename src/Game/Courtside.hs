module Game.Courtside where


import qualified Game.SFML as SFML
import Game.SFML (SFML)

import Game.Lib

import Game.Courtside.GameState
import Game.Courtside.Render (render)
import Game.Courtside.Update (update)


initialize :: SFML.Context -> SFML (SFML.LoopState GameState)
initialize ctx = do sr <- SFML.getContextScreenResolution ctx

                    -- Initialize anything else here
                    let gs = gameState sr

                    return $ SFML.LoopState render update gs


gameState :: ScreenResolution -> GameState
gameState sr =
    let cam     = defaultCamera sr
        court   = defaultCourt
        athlete = defaultAthlete
    in GameState cam court athlete False

{-# LANGUAGE RecordWildCards #-}

module Game.Courtside.Render where

import Game.SFML (SFML)
import qualified Game.SFML as SFML
import qualified SFML.Graphics as SF
import qualified SFML.System as SF

import Game.Courtside.GameState
import Game.Lib


render :: GameState -> SF.RenderWindow -> SFML ()
render m w = renderCourt m w >> renderAthlete m w

renderCourt :: GameState -> SF.RenderWindow -> SFML ()
renderCourt GameState{..} window =
    let vs       = courtVs _gsCourt
        pxCoords = map (project _gsCamera) vs

        vertices = zipWith promotePixelCoords pxCoords colors

        leftWall = take 4 $ vertices
        ground   = take 4 . drop 2 $ vertices
        rigtWall = drop 4 $ vertices

        draw strip = SFML.drawPrimitives window strip SF.TriangleStrip Nothing

    in mapM_ draw [ ground
                  , leftWall
                  , rigtWall]

renderAthlete :: GameState -> SF.RenderWindow -> SFML ()
renderAthlete GameState{..} window =
    let bottomLeft = _gsAthlete^.athPosition.pos
        topRight   = bottomLeft + V3 (_gsAthlete^.athWidth) (_gsAthlete^.athHeight) 0

        bl@(PixelCoords minX maxY) = project _gsCamera bottomLeft
        tr@(PixelCoords maxX minY) = project _gsCamera topRight
        br = PixelCoords maxX maxY
        tl = PixelCoords minX minY
        pxCoords = [bl, br, tl, tr]
        clrs = take 4 . repeat $ SF.Color 0 30 80 255
        strip = zipWith promotePixelCoords pxCoords clrs

    in SFML.drawPrimitives window strip SF.TriangleStrip Nothing

project :: Camera -> V3 -> PixelCoords
project c = perspectiveProjectToCamera c . Position

promotePixelCoords :: PixelCoords -> SF.Color -> SF.Vertex
promotePixelCoords (PixelCoords x y) c = SF.Vertex (on x y SF.Vec2f fromIntegral) c $ SF.Vec2f 0 0
  where on a b h g = h (g a) (g b)

colors :: [SF.Color]
colors =
    [ SF.Color 255 0 0 255
    , SF.Color 255 0 0 255
    , SF.Color 205 0 255 255
    , SF.Color 0 200 255 255
    , SF.Color 200 0 255 255
    , SF.Color 0 200 255 255
    , SF.Color 255 0 0 255
    , SF.Color 255 0 0 255
    ]

courtVs :: Court -> [V3]
courtVs Court{..} =
    [ V3 0 h d
    , V3 0 h 0
    , V3 0 0 d
    , V3 0 0 0
    , V3 w 0 d
    , V3 w 0 0
    , V3 w h d
    , V3 w h 0
    ]
  where d   = negate _crtDepth
        h   = _crtHeight
        w   = _crtWidth

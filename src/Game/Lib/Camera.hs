{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Lib.Camera where

import Game.Lib.Lens
import Game.Lib.Dimensions
import Game.Lib.Math

data Camera = Camera {
                _camResolution  :: ScreenResolution
              , _camFocalLength :: Double
              , _camPixelSize   :: Double
              , _camPosition    :: Position
              , _camLookAt      :: UnitDirection
              , _camRightDir    :: UnitDirection
              } deriving (Show)

makeAllLenses [ ''Camera
              ]


defaultCamera :: ScreenResolution -> Camera
defaultCamera res =
    Camera {
      _camResolution  = res
    , _camFocalLength = 6.4
    , _camPixelSize   = 0.26
    , _camPosition    = Position $ V3 14 17 1
    , _camLookAt      = unitize . Direction $ V3 0 0 (-1)
    , _camRightDir    = unitize . Direction $ V3 1 0 0
    }

rotateCameraUp :: Camera -> Degrees -> Camera
rotateCameraUp cam degrees = cam & camLookAt %~ (rotateUnitDirAboutX degrees)

rotateCameraDown :: Camera -> Degrees -> Camera
rotateCameraDown cam (Degrees d) = rotateCameraUp cam (Degrees $ negate d)

{- Positions that are "clipped" by the Camera but in front
   are returned as the coordinates that would
   correspond to an infinitely large view frustum.

   Positions that are behind the camera are returned as
   PixelCoords -10000 -10000.

   Ideally this function would return Maybe PixelCoords
   to handle these two cases, but the first case is not
   actually a bug and the latter case will never* happen
   in this particular game.

   *Well, if it becomes a problem, we'll fix it up.
-}
perspectiveProjectToCamera :: Camera -> Position -> PixelCoords
perspectiveProjectToCamera c@Camera{..} pointPosition =
    let camPos = _camPosition^.pos
        imagePlaneNormal             = _camLookAt^.udir
        Position imagePlaneCenterPos = imagePlaneCenter c
        imagePlaneCoeff              = negate $ imagePlaneNormal .*. imagePlaneCenterPos

        UnitDirection eyeRay = directionFromTo _camPosition pointPosition
        eyeNormalCosine      = eyeRay .*. imagePlaneNormal

        intersectCoeff    = negate (camPos .*. imagePlaneNormal + imagePlaneCoeff) / eyeNormalCosine
        pointOnImagePlane = Position $ camPos + (intersectCoeff .* eyeRay)

        safeCoords = projectToCameraBasis c pointOnImagePlane
        badCoords  = PixelCoords (-10000) (-10000)

    in if absLessThanEpsilon9 eyeNormalCosine || intersectCoeff <= 0
       then badCoords
       else safeCoords


imagePlaneCenter :: Camera -> Position
imagePlaneCenter Camera{..} = Position $ (_pos _camPosition) + (_udir _camLookAt *. _camFocalLength)

projectToCameraBasis :: Camera -> Position -> PixelCoords
projectToCameraBasis c@Camera{..} (Position pointOnImagePlane) =
    let Position imagePlaneCenterPos = imagePlaneCenter c
        relativePos = pointOnImagePlane - imagePlaneCenterPos

        basis1 = _udir _camRightDir
        basis2 = basis1 `cross` (_udir _camLookAt)

        displacementAlongX = basis1 .*. relativePos
        displacementAlongY = basis2 .*. relativePos

        pixelDispX, pixelDispY :: Int
        pixelDispX = round $ displacementAlongX / _camPixelSize
        pixelDispY = round $ displacementAlongY / _camPixelSize

        {- Now we have how far the target point is from the center
           of the screen in pixel distances, we just need to convert
           that to screen coordinates. Negative values are fine!
           SFML (OpenGL) will take care of clipping.
        -}
        ScreenResolution (Pixels width) (Pixels height) = _camResolution
        x = pixelDispX + width `div` 2
        y = height `div` 2 - pixelDispY

   in PixelCoords x y

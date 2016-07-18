{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Game.Lib.Dimensions where

import Game.Lib.Lens

------------
-- Screen stuff
------------

newtype Pixels = Pixels {_px :: Int} deriving (Show, Num)

data PixelCoords = PixelCoords {
                     _pxcX :: !Int
                   , _pxcY :: !Int
                   }
                   deriving (Show)

data ScreenResolution = ScreenResolution {
                          _scrWidth  :: !Pixels
                        , _scrHeight :: !Pixels
                        } deriving (Show)
------------
-- Vector geometry
------------

data V3 = V3 {
            _v3x :: !Double
          , _v3y :: !Double
          , _v3z :: !Double
          }
          deriving (Show)

instance Num V3 where
  (V3 u v w) + (V3 x y z) = V3 (u+x) (v+y) (w+z)
  {-# INLINE (+) #-}
  (V3 u v w) - (V3 x y z) = V3 (u-x) (v-y) (w-z)
  {-# INLINE (-) #-}
  (V3 u v w) * (V3 x y z) = V3 (u*x) (v*y) (w*z)
  {-# INLINE (*) #-}
  abs    (V3 x y z)       = V3 (abs x) (abs y) (abs z)
  {-# INLINE abs #-}
  signum (V3 x y z)       = V3 (signum x) (signum y) (signum z)
  {-# INLINE signum #-}
  fromInteger x           = V3 x' x' x'
    where x' = fromInteger x
  {-# INLINE fromInteger #-}

newtype Position      = Position {_pos :: V3} deriving (Show)
newtype Velocity      = Velocity {_vel :: V3} deriving (Show)
newtype UnitDirection = UnitDirection {_udir :: V3} deriving (Show)
newtype Direction     = Direction {_dir :: V3} deriving (Show)

newtype Degrees = Degrees {_deg :: Double} deriving (Show, Num)
newtype Radians = Radians {_rad :: Double} deriving (Show, Num)


makeAllLenses [ ''Pixels
              , ''PixelCoords
              , ''ScreenResolution
              , ''V3

              , ''Position
              , ''Velocity
              , ''UnitDirection
              , ''Direction

              , ''Degrees
              , ''Radians
              ]

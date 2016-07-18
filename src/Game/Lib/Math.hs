module Game.Lib.Math where

import Game.Lib.Dimensions

-- Multiplication by a scalar
(.*) :: Double -> V3 -> V3
k .* (V3 x y z) = V3 (k*x) (k*y) (k*z)
{-# INLINE (.*) #-}

-- Multiplication by a scalar flipped argument order
(*.) :: V3 -> Double -> V3
(V3 x y z) *. k = V3 (k*x) (k*y) (k*z)
{-# INLINE (*.) #-}

-- Division by a scalar
(/.) :: V3 -> Double -> V3
(V3 x y z) /. k = V3 (x / k) (y / k) (z / k)
{-# INLINE (/.) #-}

-- Dot product
(.*.) :: V3 -> V3 -> Double
(V3 xl yl zl) .*. (V3 xr yr zr) = xl*xr + yl*yr + zl*zr
{-# INLINE (.*.) #-}

-- Cross product
cross :: V3 -> V3 -> V3
cross (V3 u1 u2 u3) (V3 v1 v2 v3) = V3 (u2*v3 - u3*v2) (u3*v1 - u1*v3) (u1*v2 - u2*v1)
{-# INLINE cross #-}

-- Vector magnitude
magnitude :: V3 -> Double
magnitude (V3 x y z) = sqrt (x*x + y*y + z*z)
{-# INLINE magnitude #-}

-- Vector magnitude squared
sqrMagnitude :: V3 -> Double
sqrMagnitude (V3 x y z) = x*x + y*y + z*z
{-# INLINE sqrMagnitude #-}

-- Normalize vector to unit length
normalize :: V3 -> V3
normalize v = v /. magnitude v
{-# INLINE normalize #-}

unitize :: Direction -> UnitDirection
unitize (Direction d) = UnitDirection (normalize d)

rotateUnitDirAboutX :: Degrees -> UnitDirection -> UnitDirection
rotateUnitDirAboutX (Degrees d) (UnitDirection (V3 x y z)) =
    let rads = d * pi / 180.0
        cost = cos rads
        sint = sin rads
    in UnitDirection $ V3 x (cost * y + sint * negate z) (sint * y + cost * z)

angleBetween :: UnitDirection -> UnitDirection -> Degrees
angleBetween (UnitDirection u) (UnitDirection v) = Degrees $ acos (u.*.v) * (180 / pi)

directionFromTo :: Position -> Position -> UnitDirection
directionFromTo (Position a) (Position b) = unitize $ Direction (b - a)

--------------------------
-- Angle stuff
--------------------------

degreeTan :: Degrees -> Double
degreeTan (Degrees t) = tan (t * pi / 180)


---------------------------
-- Numerical stuff
---------------------------

absLessThanEpsilon9 :: (Ord a, Floating a) => a -> Bool
absLessThanEpsilon9 d = abs d <= 1e-9

{-# LANGUAGE Safe #-}

module Math.Quaternion (conjugate, multiply, normalize, quaternion) where


import Math.Data

import Prelude hiding (length)


-- Type aliases.
type Radians a = a

conjugate :: Floating a => Quaternion a -> Quaternion a
conjugate (Quaternion x y z w) = Quaternion x' y' z' w
    where
        x'  = negate x
        y'  = negate y
        z'  = negate z

length :: Floating a => Quaternion a -> a
length (Quaternion x y z w) = sqrt $ x' + y' + z' + w'
    where
        x'  = x * x
        y'  = y * y
        z'  = z * z
        w'  = w * w

multiply :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
multiply (Quaternion x1 y1 z1 w1) (Quaternion x2 y2 z2 w2) = Quaternion x y z w
    where
        x = w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2
        y = w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2
        z = w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2
        w = w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2

normalize :: Floating a => Quaternion a -> Quaternion a
normalize q@(Quaternion x y z w) = Quaternion x' y' z' w'
    where
        l   = length q
        x'  = x / l
        y'  = y / l
        z'  = z / l
        w'  = w / l

quaternion :: Floating a => Vertex3 a -> Radians a -> Quaternion a
quaternion (Vertex3 x y z) r = Quaternion x' y' z' w
    where
        r'  = r / 2
        s   = sin r'
        w   = cos r'
        x'  = x * s
        y'  = y * s
        z'  = z * s
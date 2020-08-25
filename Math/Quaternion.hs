{-# LANGUAGE Safe #-}

module Math.Quaternion (conjugate, multiply) where


import Math.Data


conjugate :: Floating a => Quaternion a -> Quaternion a
conjugate (Quaternion x y z w) = Quaternion x' y' z' w
    where
        x'  = negate x
        y'  = negate y
        z'  = negate z

multiply :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
multiply (Quaternion x1 y1 z1 w1) (Quaternion x2 y2 z2 w2) = Quaternion x y z w
    where
        x = w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2
        y = w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2
        z = w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2
        w = w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2
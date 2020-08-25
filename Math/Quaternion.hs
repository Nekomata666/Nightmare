{-# LANGUAGE Safe #-}

module Math.Quaternion (conjugate) where


import Math.Data


conjugate :: Floating a => Quaternion a -> Quaternion a
conjugate (Quaternion x y z w) = Quaternion x' y' z' w
    where
        x'  = negate x
        y'  = negate y
        z'  = negate z
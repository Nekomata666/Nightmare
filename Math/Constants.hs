{-# LANGUAGE DuplicateRecordFields, Safe #-}

module Math.Constants where


import Math.Data


matrix4Identity :: Num a => Matrix4 a
matrix4Identity = Matrix4 1 0 0 0
                          0 1 0 0
                          0 0 1 0
                          0 0 0 1
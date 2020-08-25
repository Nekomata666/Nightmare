{-# LANGUAGE DuplicateRecordFields, Safe #-}

module Math.Data where


data Matrix3 a = Matrix3 a a a
                         a a a
                         a a a

data Matrix4 a = Matrix4 a a a a
                         a a a a
                         a a a a
                         a a a a

data Quaternion a = Quaternion{
    x :: a,
    y :: a,
    z :: a,
    w :: a
}

data Scalar a = Scalar a

data UBO a = UBO{
    axis :: Quaternion a,
    conj :: Quaternion a
}

data Vertex2 a = Vertex2{
    x :: a,
    y :: a
}

data Vertex3 a = Vertex3{
    x :: a,
    y :: a,
    z :: a
}

data Vertex4 a = Vertex4{
    x :: a,
    y :: a,
    z :: a,
    w :: a
}
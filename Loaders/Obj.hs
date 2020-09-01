{-# LANGUAGE Safe #-}

module Loaders.Obj (Model(..), loadObj) where


import Data.ByteString.Char8 as C (unpack, readFile)
import Data.Word (Word32)

import Math.Data (Vertex3(Vertex3))

data Model = Model{
    vertices    :: [Vertex3 Float],
    normals     :: [Vertex3 Float],
    indices     :: [Word32]
}

loadObj :: FilePath -> IO Model
loadObj f = do
    s <- C.readFile f
    let l = lines $ C.unpack s
        n = munchN l
        i = map (\x -> x - 1) $ munchF l
        v = munchV l
    return $ Model v n i

munchF :: [String] -> [Word32]
munchF []       = []
munchF (x:s)
    | x' == "f" = indices ++ munchF s
    | otherwise = munchF s
    where
        (x':xs) = words x
        indices = breakFace xs

breakFace :: [String] -> [Word32]
breakFace (x:y:z:[]) = [x', y', z']
    where
        x' = read $ takeWhile (/= '/') x
        y' = read $ takeWhile (/= '/') y
        z' = read $ takeWhile (/= '/') z

munchN :: [String] -> [Vertex3 Float]
munchN []       = []
munchN (x:s)
    | x' == "vn" = normal : munchN s
    | otherwise = munchN s
    where
        (x':xs) = words x
        normal  = breakNormal xs

breakNormal :: [String] -> Vertex3 Float
breakNormal (x:y:z:[]) = Vertex3 x' y' z'
    where
        x' = read x :: Float
        y' = read y :: Float
        z' = read z :: Float

munchV :: [String] -> [Vertex3 Float]
munchV []       = []
munchV (x:s)
    | x' == "v" = vertex : munchV s
    | otherwise = munchV s
    where
        (x':xs) = words x
        vertex  = breakVertex xs

breakVertex :: [String] -> Vertex3 Float
breakVertex (x:y:z:[]) = Vertex3 x' y' z'
    where
        x' = read x :: Float
        y' = read y :: Float
        z' = read z :: Float
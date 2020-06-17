{-# LANGUAGE Safe #-}

module Graphics.Utilities where


import Data.Bits (shiftL, (.|.))
import Data.Foldable (foldlM)
import Data.Maybe
import Data.Word (Word32)

import Foreign
import Foreign.C.Types
import Foreign.C.String


type Major      = Word32
type Minor      = Word32
type Patch      = Word32
type Version    = Word32


cast :: (Read a, Show b) => b -> a
cast a = read $ show a

fromMaybeIO :: Storable a => Maybe a -> IO (Ptr a)
fromMaybeIO m
    | isNothing m = return nullPtr
    | otherwise   = alloca $ \p -> do
        poke p $ fromJust m
        return p

fromMaybeListIO :: (Num a, Show a, Storable b) => a -> Maybe [b] -> IO (Ptr b)
fromMaybeListIO c m
    | isNothing m = return nullPtr
    | otherwise   = allocaArray i $ \p -> do
        pokeArray p $ fromJust m
        return p
    where
        i = cast c

fromMaybeStringListIO :: (Num a, Show a) => a -> Maybe [String] -> IO (Ptr CString)
fromMaybeStringListIO c m
    | isNothing m = return nullPtr
    | otherwise   = allocaArray i $ \p -> do
        m' <- stringListToCStringList $ fromJust m
        pokeArray p m'
        return p
    where
        i = cast c

makeAPI :: Major -> Minor -> Patch -> Version
makeAPI major minor patch = shiftL major 22 .|. shiftL minor 12 .|. patch

stringListToCStringList :: [String] -> IO [CString]
stringListToCStringList [] = return []
stringListToCStringList s = foldlM helper [] s
    where
        helper cs s' = do
            ncs <- newCString s'
            return $ cs ++ [ncs]

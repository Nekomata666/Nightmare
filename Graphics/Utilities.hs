{-# LANGUAGE Safe #-}

module Graphics.Utilities where


import Data.Bits (shiftL, (.|.))
import Data.Word (Word32)


type Major      = Word32
type Minor      = Word32
type Patch      = Word32
type Version    = Word32


makeAPI :: Major -> Minor -> Patch -> Version
makeAPI major minor patch = shiftL major 22 .|. shiftL minor 12 .|. patch

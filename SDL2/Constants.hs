{-# LANGUAGE Safe #-}

module SDL2.Constants where


import Data.Int (Int32)
import Data.Word (Word32)


init_Videos :: Word32
init_Videos = 0x00000020

window_Shown :: Int32
window_Shown = 0x00000004
window_Vulkan :: Int32
window_Vulkan = 0x10000000

windowPos_Undefined :: Word32
windowPos_Undefined = 0x1FFF0000

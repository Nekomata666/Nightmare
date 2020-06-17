{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module SDL2.SDL2 where


import Data.Int     (Int32)
import Data.Word    (Word32)

import Foreign


foreign import ccall unsafe "SDL_Init"
    c_SDL_Init :: Word32 -> IO Int32

sdl2Init :: Word32 -> IO Int32
sdl2Init = c_SDL_Init

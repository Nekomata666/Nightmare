{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module SDL2.SDL2 (sdl2Init, sdl2Quit) where


import Data.Int     (Int32)
import Data.Word    (Word32)

import Foreign


foreign import ccall unsafe "SDL_Init"
    c_SDL_Init :: Word32 -> IO Int32

foreign import ccall unsafe "SDL_Quit"
    c_SDL_Quit :: IO ()

sdl2Init :: Word32 -> IO Int32
sdl2Init = c_SDL_Init

sdl2Quit :: IO ()
sdl2Quit = c_SDL_Quit

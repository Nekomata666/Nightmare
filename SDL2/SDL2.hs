{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module SDL2.SDL2 (sdl2CreateWindow, sdl2Init, sdl2Quit) where


import Data.Int     (Int32)
import Data.Word    (Word32)

import Foreign
import Foreign.C.String


data SDL_Window

-- Type aliases.
type Height = Int32
type HWindow = Ptr SDL_Window
type Width = Int32


foreign import ccall unsafe "SDL_CreateWindow"
    c_SDL_CreateWindow :: CString -> Word32 -> Word32 -> Width -> Height -> Int32 -> IO HWindow

foreign import ccall unsafe "SDL_Init"
    c_SDL_Init :: Word32 -> IO Int32

foreign import ccall unsafe "SDL_Quit"
    c_SDL_Quit :: IO ()

sdl2CreateWindow :: String -> Word32 -> Word32 -> Width -> Height -> Int32 -> IO HWindow
sdl2CreateWindow n x0 y0 x1 y1 f = do
    cs <- newCString n
    c_SDL_CreateWindow cs x0 y0 x1 y1 f

sdl2Init :: Word32 -> IO Int32
sdl2Init = c_SDL_Init

sdl2Quit :: IO ()
sdl2Quit = c_SDL_Quit

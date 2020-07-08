{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module SDL2.SDL2 (sdl2CreateWindow, sdl2DestroyWindow, sdl2Init, sdl2VulkanCreateSurface, sdl2Quit) where


import Data.Int     (Int32)
import Data.Word    (Word32)

import Foreign
import Foreign.C.String

import Graphics.Vulkan.Types (VkInstance(..), VkSurfaceKHR(..))


data SDL_Window

-- Type aliases.
type Height     = Int32
type HWindow    = Ptr SDL_Window
type Width      = Int32


foreign import ccall unsafe "SDL_CreateWindow"
    c_SDL_CreateWindow :: CString -> Word32 -> Word32 -> Width -> Height -> Int32 -> IO HWindow

foreign import ccall unsafe "SDL_DestroyWindow"
    c_SDL_DestroyWindow :: HWindow -> IO ()

foreign import ccall unsafe "SDL_Init"
    c_SDL_Init :: Word32 -> IO Int32

foreign import ccall unsafe "SDL_Vulkan_CreateSurface"
    c_SDL_Vulkan_CreateSurface :: HWindow -> VkInstance -> Ptr VkSurfaceKHR -> IO Bool

foreign import ccall unsafe "SDL_Quit"
    c_SDL_Quit :: IO ()

sdl2CreateWindow :: String -> Word32 -> Word32 -> Width -> Height -> Int32 -> IO HWindow
sdl2CreateWindow n x0 y0 x1 y1 f = do
    cs <- newCString n
    c_SDL_CreateWindow cs x0 y0 x1 y1 f

sdl2DestroyWindow :: HWindow -> IO ()
sdl2DestroyWindow = c_SDL_DestroyWindow

sdl2Init :: Word32 -> IO Int32
sdl2Init = c_SDL_Init

sdl2VulkanCreateSurface :: HWindow -> VkInstance -> IO VkSurfaceKHR
sdl2VulkanCreateSurface hW vI = alloca $ \p -> do
    _ <- c_SDL_Vulkan_CreateSurface hW vI p
    peek p

sdl2Quit :: IO ()
sdl2Quit = c_SDL_Quit

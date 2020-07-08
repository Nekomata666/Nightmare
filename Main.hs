{-# LANGUAGE Safe #-}

module Main (main) where


import Control.Concurrent

import Graphics.Vulkan

import SDL2.Constants
import SDL2.SDL2


main :: IO ()
main = do
    _   <- sdl2Init init_Videos
    hW  <- sdl2CreateWindow "Nightmare" windowPos_Undefined windowPos_Undefined 1600 900 window_Vulkan
    vkInst <- createInstance
    vkSurf <- sdl2VulkanCreateSurface hW vkInst

    ----------------------------------------------------------------------------------------------------------------------------
    --
    -- Vulkan
    --
    ----------------------------------------------------------------------------------------------------------------------------
    initialize vkInst

    threadDelay 9000000


    ----------------------------------------------------------------------------------------------------------------------------
    --
    -- Shutdown
    --
    ----------------------------------------------------------------------------------------------------------------------------
    sdl2DestroyWindow hW
    sdl2Quit

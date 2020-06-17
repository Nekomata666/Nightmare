{-# LANGUAGE Safe #-}

module Main (main) where


import Control.Concurrent

import Constants
import SDL2


main :: IO ()
main = do
    _   <- sdl2Init init_Videos
    hW  <- sdl2CreateWindow "Nightmare" windowPos_Undefined windowPos_Undefined 1600 900 window_Vulkan

    threadDelay 9000000

    sdl2DestroyWindow hW
    sdl2Quit

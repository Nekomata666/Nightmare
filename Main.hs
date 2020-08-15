{-# LANGUAGE Safe #-}

module Main (main) where


import Control.Concurrent
import Control.Monad

import Graphics.Vulkan
import Graphics.Vulkan.Types

import SDL2.Constants
import SDL2.Data
import SDL2.Enumerations
import SDL2.SDL2


main :: IO ()
main = do
    _       <- sdl2Init init_Videos
    hW      <- sdl2CreateWindow "Nightmare" windowPos_Undefined windowPos_Undefined 1600 900 window_Vulkan
    vkInst  <- createInstance
    vkSurf  <- sdl2VulkanCreateSurface hW vkInst

    ----------------------------------------------------------------------------------------------------------------------------
    --
    -- Vulkan
    --
    ----------------------------------------------------------------------------------------------------------------------------
    (buffer, vkCoBu, vkCPo0, vkDeP0, vkDSL0, vkDev0, memory, fences, vFrame, vkIma0, swapIV, pipe, vkPiCa, pipeLa, vkQue0, vkRePa, sema, vkSC) <- initialize vkInst vkSurf

    loop False sdlFirstEvent vkDev0 fences vkSC sema vkCoBu vkQue0 0


    ----------------------------------------------------------------------------------------------------------------------------
    --
    -- Shutdown
    --
    ----------------------------------------------------------------------------------------------------------------------------
    shutdown buffer vkCPo0 vkDeP0 vkDSL0 vkDev0 memory fences vFrame vkIma0 swapIV vkInst pipe vkPiCa pipeLa vkQue0 vkRePa sema vkSurf vkSC
    sdl2DestroyWindow hW
    sdl2Quit

loop :: Bool -> SDLEventType -> VkDevice -> [VkFence] -> VkSwapchainKHR -> ([VkSemaphore], [VkSemaphore]) -> [VkCommandBuffer] -> VkQueue -> Frame -> IO ()
loop True (SDLEventType 256) _ _ _ _ _ _ _ = return ()
loop _ _ vkDev0 fences vkSC sema vkCoBu vkQue0 f = do
    a@(r, e) <- sdl2PollEvent
    when r $ print a
    f' <- draw vkDev0 fences vkSC sema vkCoBu vkQue0 f
    loop r (SDLEventType $ eType e) vkDev0 fences vkSC sema vkCoBu vkQue0 f'
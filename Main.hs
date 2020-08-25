{-# LANGUAGE Safe #-}

module Main (main) where


import Control.Concurrent
import Control.Monad

import Data.Word (Word8)

import Foreign (Storable)

import Graphics.Vulkan
import Graphics.Vulkan.Types

import Math.Data
import Math.Quaternion

import SDL2.Constants
import SDL2.Data
import SDL2.Enumerations
import SDL2.SDL2

step :: Float
step = 0.0174532925199

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
    (buffer, vkCoBu, vkCPo0, vkDeP0, deSLay, vkDev0, memory, uniMe, fences, vFrame, vkIma0, swapIV, pipe, vkPiCa, pipeLa, vkQue0, vkRePa, sema, vkSC) <- initialize vkInst vkSurf

    loop False sdlFirstEvent vkDev0 uniMe fences vkSC sema vkCoBu vkQue0 0 u 0


    ----------------------------------------------------------------------------------------------------------------------------
    --
    -- Shutdown
    --
    ----------------------------------------------------------------------------------------------------------------------------
    shutdown buffer vkCPo0 vkDeP0 deSLay vkDev0 (memory ++ uniMe) fences vFrame vkIma0 swapIV vkInst pipe vkPiCa pipeLa vkQue0 vkRePa sema vkSurf vkSC
    sdl2DestroyWindow hW
    sdl2Quit
    where
        a = quaternion (Vertex3 0 0 0) step
        c = conjugate a
        u = UBO a c

loop :: (Floating a, Storable a) => Bool -> SDLEventType -> VkDevice -> [VkDeviceMemory] -> [VkFence] -> VkSwapchainKHR -> ([VkSemaphore], [VkSemaphore]) -> [VkCommandBuffer] -> VkQueue -> Frame -> UBO a -> Float -> IO ()
loop True (SDLEventType 256) _ _ _ _ _ _ _ _ _ _ = return ()
loop _ _ vkDev0 uniMe fences vkSC sema vkCoBu vkQue0 f ubo s = do
    a@(r, e) <- sdl2PollEvent
    k <- sdl2GetKeyboardState
    let s' = actions k s
    -- when r $ print a
    f' <- draw vkDev0 uniMe fences vkSC sema vkCoBu vkQue0 f ubo
    loop r (SDLEventType $ eType e) vkDev0 uniMe fences vkSC sema vkCoBu vkQue0 f' u s'
    where
        a = quaternion (Vertex3 0 0 1) $ s * step
        c = conjugate a
        u = UBO a c

actions :: [Word8] -> Float -> Float
actions k s
    | k !! (fromIntegral $ unSDLScancode sdlScancodeA) == 1 = s - 64
    | k !! (fromIntegral $ unSDLScancode sdlScancodeD) == 1 = s + 64
    | otherwise = s
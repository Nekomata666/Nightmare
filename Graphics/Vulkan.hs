{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan.Devices
import Graphics.Vulkan.Instance


initialize :: IO ()
initialize = do
    let api = makeAPI 1 2 141
    app     <- createVkApplicationInfo nullPtr "Nightmare" 0 "Nightmare" 0 api
    vkInfo  <- createVkInstanceCreateInfo nullPtr 0 (Just app) 1
        (Just ["VK_LAYER_KHRONOS_validation"]) 2
        (Just ["VK_EXT_debug_report", "VK_KHR_surface"])
    vkIn <- vkCreateInstance vkInfo

    return ()

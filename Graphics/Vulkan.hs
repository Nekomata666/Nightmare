{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan.Devices
import Graphics.Vulkan.Instance
import Graphics.Vulkan.Types


initialize :: IO ()
initialize = do
    let api = makeAPI 1 2 141
    appInf  <- createVkApplicationInfo nullPtr "Nightmare" 0 "Nightmare" 0 api
    vkInfo  <- createVkInstanceCreateInfo nullPtr 0 (Just appInf) 1
        (Just ["VK_LAYER_KHRONOS_validation"]) 2
        (Just ["VK_EXT_debug_report", "VK_KHR_surface"])
    vkInst  <- vkCreateInstance vkInfo
    physDe  <- vkEnumeratePhysicalDevices $ fst vkInst
    let d0  = head physDe
    physFe  <- vkGetPhysicalDeviceFeatures d0
    queuIn  <- vkCreateDeviceQueueInfo nullPtr (VkDeviceQueueCreateFlags 0) 0 1 [1.0]
    devInf  <- vkCreateDeviceInfo nullPtr (VkDeviceCreateFlags 0) 1 queuIn 1 ["VK_KHR_swapchain"] physFe

    return ()

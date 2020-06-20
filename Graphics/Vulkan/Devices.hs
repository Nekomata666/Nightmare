{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Devices (vkEnumeratePhysicalDevices, vkGetPhysicalDeviceFeatures) where


import Data.Word (Word32)

import Foreign
import Foreign.C.String

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkEnumeratePhysicalDevices"
    c_vkEnumeratePhysicalDevices :: VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult

foreign import ccall unsafe "vkGetPhysicalDeviceFeatures"
    c_vkGetPhysicalDeviceFeatures :: VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ()

vkEnumeratePhysicalDevices :: VkInstance -> IO [VkPhysicalDevice]
vkEnumeratePhysicalDevices vkInst = do
        n <- firstPass
        secondPass n
        where
            firstPass = alloca $ \p -> do
                _ <- c_vkEnumeratePhysicalDevices vkInst p nullPtr
                peek p
            secondPass n = alloca $ \pN ->
                allocaArray i $ \pPD -> do
                    poke pN n
                    _ <- c_vkEnumeratePhysicalDevices vkInst pN pPD
                    peekArray i pPD
                    where
                        i = cast n

vkGetPhysicalDeviceFeatures :: VkPhysicalDevice -> IO VkPhysicalDeviceFeatures
vkGetPhysicalDeviceFeatures pD = alloca $ \pF -> do
    c_vkGetPhysicalDeviceFeatures pD pF
    peek pF
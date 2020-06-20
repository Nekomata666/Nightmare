{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Devices (vkCreateDeviceInfo, vkCreateDeviceQueueInfo, vkEnumeratePhysicalDevices, vkGetPhysicalDeviceFeatures) where


import Data.Void (Void)
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

vkCreateDeviceInfo :: Ptr Void -> VkDeviceCreateFlags -> Word32 -> VkDeviceQueueCreateInfo -> Word32 -> [String] ->
    VkPhysicalDeviceFeatures -> IO VkDeviceCreateInfo
vkCreateDeviceInfo v fl qC dQCI eC e fe =
    alloca $ \pQ ->
        alloca $ \pF ->
            allocaArray i $ \pE -> do
                e' <- stringListToCStringList e
                poke pQ dQCI
                poke pF fe
                pokeArray pE e'
                return $ VkDeviceCreateInfo structureTypeDeviceCreateInfo v fl qC pQ 0 nullPtr eC pE pF
                where
                    i = cast eC

vkCreateDeviceQueueInfo :: Ptr Void -> VkDeviceQueueCreateFlags -> Word32 -> Word32 -> [Float] -> IO VkDeviceQueueCreateInfo
vkCreateDeviceQueueInfo v f fI c p = allocaArray i $ \pP -> do
    pokeArray pP p
    return $ VkDeviceQueueCreateInfo structureTypeDeviceQueueCreateInfo v f fI c pP
    where
        i = cast c

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
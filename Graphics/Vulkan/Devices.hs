{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Devices (createVkDeviceCreateInfo, createVkDeviceQueueCreateInfo, vkCreateDevice, vkDestroyDevice, vkDeviceWaitIdle, vkEnumeratePhysicalDevices, vkGetDeviceQueue, vkGetPhysicalDeviceFeatures2, vkGetPhysicalDeviceSurfaceSupport) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign
import Foreign.C.String

import Graphics.Utilities

import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type QueueCount         = Word32
type QueueFamily        = Word32
type QueueFamilyIndex   = Word32
type QueueIndex         = Word32
type QueuePriorities    = [Float]


foreign import ccall unsafe "vkCreateDevice"
    c_vkCreateDevice :: VkPhysicalDevice -> Ptr VkDeviceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDevice -> IO VkResult

foreign import ccall unsafe "vkDestroyDevice"
    c_vkDestroyDevice :: VkDevice -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkDeviceWaitIdle"
    c_vkDeviceWaitIdle :: VkDevice -> IO VkResult

foreign import ccall unsafe "vkEnumeratePhysicalDevices"
    c_vkEnumeratePhysicalDevices :: VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult

foreign import ccall unsafe "vkGetDeviceQueue"
    c_vkGetDeviceQueue :: VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()

foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2"
    c_vkGetPhysicalDeviceFeatures2 :: VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures2 -> IO ()

foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceSupportKHR"
    c_vkGetPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr VkBool -> IO VkResult

createVkDeviceQueueCreateInfo :: Next -> VkDeviceQueueCreateFlags -> QueueFamilyIndex -> QueueCount -> QueuePriorities ->
    IO VkDeviceQueueCreateInfo
createVkDeviceQueueCreateInfo v f fI c p = allocaArray i $ \pP -> do
    pokeArray pP p
    return $ VkDeviceQueueCreateInfo structureTypeDeviceQueueCreateInfo v f fI c pP
    where
        i = cast c

createVkDeviceCreateInfo :: Next -> VkDeviceCreateFlags -> Word32 -> VkDeviceQueueCreateInfo -> Word32 -> [String] ->
    Maybe VkPhysicalDeviceFeatures -> IO VkDeviceCreateInfo
createVkDeviceCreateInfo v fl qC dQCI eC e fe =
    alloca $ \pQ ->
        allocaArray i $ \pE -> do
            e' <- stringListToCStringList e
            poke pQ dQCI
            pF <- fromMaybeIO fe
            pokeArray pE e'
            return $ VkDeviceCreateInfo structureTypeDeviceCreateInfo v fl qC pQ 0 nullPtr eC pE pF
            where
                i = cast eC

createVkPhysicalDeviceFeatures2 :: IO (Ptr VkPhysicalDeviceFeatures2)
createVkPhysicalDeviceFeatures2 = alloca $ \p -> do
    pV11F <- createVkPhysicalDeviceVulkan11Features
    poke p $ VkPhysicalDeviceFeatures2 structureTypePhysicalDeviceFeatures2 (castPtr pV11F) pDF
    return p
    where
        pDF = VkPhysicalDeviceFeatures vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse

createVkPhysicalDeviceVulkan11Features :: IO (Ptr VkPhysicalDeviceVulkan11Features)
createVkPhysicalDeviceVulkan11Features = alloca $ \p -> do
    n <- createVkPhysicalDeviceVulkan12Features
    poke p $ pV11F n
    return p
    where
        pV11F n = VkPhysicalDeviceVulkan11Features structureTypePhysicalDeviceVulkan11Features (castPtr n) vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse

createVkPhysicalDeviceVulkan12Features:: IO (Ptr VkPhysicalDeviceVulkan12Features)
createVkPhysicalDeviceVulkan12Features = alloca $ \p -> do
    poke p pV12F
    return p
    where
        pV12F = VkPhysicalDeviceVulkan12Features structureTypePhysicalDeviceVulkan12Features nullPtr vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse vkFalse

vkCreateDevice :: VkPhysicalDevice -> VkDeviceCreateInfo -> IO VkDevice
vkCreateDevice physical dCI = alloca $ \pDCI ->
    alloca $ \pPD -> do
        poke pDCI dCI
        _ <- c_vkCreateDevice physical pDCI nullPtr pPD
        peek pPD

vkDestroyDevice :: VkDevice -> IO ()
vkDestroyDevice d = c_vkDestroyDevice d nullPtr

vkDeviceWaitIdle :: VkDevice -> IO VkResult
vkDeviceWaitIdle = c_vkDeviceWaitIdle

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

vkGetDeviceQueue :: VkDevice -> QueueFamily -> QueueIndex -> IO VkQueue
vkGetDeviceQueue d qFI qI = alloca $ \p -> do
    c_vkGetDeviceQueue d qFI qI p
    peek p

vkGetPhysicalDeviceFeatures2 :: VkPhysicalDevice -> IO (Ptr VkPhysicalDeviceFeatures2)
vkGetPhysicalDeviceFeatures2 pD = do
    pF <- createVkPhysicalDeviceFeatures2
    c_vkGetPhysicalDeviceFeatures2 pD pF
    return pF

vkGetPhysicalDeviceSurfaceSupport :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> IO VkBool
vkGetPhysicalDeviceSurfaceSupport pD qIndex s = alloca $ \p -> do
    _ <- c_vkGetPhysicalDeviceSurfaceSupportKHR pD qIndex s p
    peek p
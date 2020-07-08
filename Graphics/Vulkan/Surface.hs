{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Surface (createVkSwapchainCreateInfo, vkCreateSwapchainKHR, vkDestroySurfaceKHR) where


import Data.Void (Void)
import Data.Word (Word32, Word64)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type Clipped                = VkBool
type ImageArrayLayers       = Word32
type MinImageCount          = Word32
type QueueFamilyIndexCount  = Word32
type QueueFamilyIndices     = Word32


foreign import ccall unsafe "vkCreateSwapchainKHR"
    c_vkCreateSwapchainKHR :: VkDevice -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR ->
        IO VkResult

foreign import ccall unsafe "vkDestroySurfaceKHR"
    c_vkDestroySurfaceKHR :: VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ()

createVkSwapchainCreateInfo :: Next -> VkSwapchainCreateFlagsKHR -> VkSurfaceKHR -> MinImageCount -> VkFormat ->
    VkColorSpaceKHR -> VkExtent2D -> ImageArrayLayers -> VkImageUsageFlagBits -> VkSharingMode -> QueueFamilyIndexCount ->
    [QueueFamilyIndices] -> VkSurfaceTransformFlagBitsKHR -> VkCompositeAlphaFlagBitsKHR -> VkPresentModeKHR -> Clipped ->
    VkSwapchainKHR -> IO VkSwapchainCreateInfoKHR
createVkSwapchainCreateInfo v sCF s mIC f cS e iAL iUFB sM qFIC qFI sTF cAF pM b sc =
    allocaArray i $ \p -> do
        pokeArray p qFI
        return $ VkSwapchainCreateInfoKHR structureTypeSwapchainCreateInfoKHR v sCF s mIC f cS e iAL u sM qFIC p sTF cAF pM b sc
        where
            i = cast qFIC
            u = VkImageUsageFlags $ unVkImageUsageFlagBits iUFB

vkCreateSwapchainKHR :: VkDevice -> VkSwapchainCreateInfoKHR -> IO VkSwapchainKHR
vkCreateSwapchainKHR d sCI = alloca $ \pSCI ->
    alloca $ \pS -> do
        poke pSCI sCI
        _ <- c_vkCreateSwapchainKHR d pSCI nullPtr pS
        peek pS

vkDestroySurfaceKHR :: VkInstance -> VkSurfaceKHR -> IO ()
vkDestroySurfaceKHR vkI vkS = c_vkDestroySurfaceKHR vkI vkS nullPtr
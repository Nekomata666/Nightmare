{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Surface (createVkSwapchainCreateInfo, vkCreateSwapchainKHR, vkDestroySurfaceKHR, vkDestroySwapchainKHR, vkGetSwapchainImagesKHR) where


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

foreign import ccall unsafe "vkDestroySwapchainKHR"
    c_vkDestroySwapchainKHR :: VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkGetSwapchainImagesKHR"
    c_vkGetSwapchainImagesKHR :: VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO ()

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
vkDestroySurfaceKHR i s = c_vkDestroySurfaceKHR i s nullPtr

vkDestroySwapchainKHR :: VkDevice -> VkSwapchainKHR -> IO ()
vkDestroySwapchainKHR d s = c_vkDestroySwapchainKHR d s nullPtr

vkGetSwapchainImagesKHR :: VkDevice -> VkSwapchainKHR -> IO [VkImage]
vkGetSwapchainImagesKHR d s = do
    n <- firstPass
    secondPass n
    where
        firstPass = alloca $ \p -> do
            _ <- c_vkGetSwapchainImagesKHR d s p nullPtr
            peek p
        secondPass n = alloca $ \pN ->
            allocaArray i $ \pI -> do
                poke pN n
                _ <- c_vkGetSwapchainImagesKHR d s pN pI
                peekArray i pI
                where
                    i = cast n
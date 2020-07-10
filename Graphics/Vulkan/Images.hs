{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Images (createVkImageCreateInfo, createVkImageSubresource, vkBindImageMemory, vkCreateImage, vkCreateImageView, vkDestroyImage, vkGetImageMemoryRequirements, vkGetImageSubresourceLayout) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkBindImageMemory"
    c_vkBindImageMemory :: VkDevice  -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

foreign import ccall unsafe "vkCreateImage"
    c_vkCreateImage :: VkDevice -> Ptr VkImageCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult

foreign import ccall unsafe "vkCreateImageView"
    c_vkCreateImageView :: VkDevice -> Ptr VkImageViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult

foreign import ccall unsafe "vkDestroyImage"
    c_vkDestroyImage :: VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkGetImageMemoryRequirements"
    c_vkGetImageMemoryRequirements :: VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()

foreign import ccall unsafe "vkGetImageSubresourceLayout"
    c_vkGetImageSubresourceLayout :: VkDevice -> VkImage -> Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()

-- Note: ImageLayout needs to be imageLayoutUndefined or imageLayoutPreinitialized
createVkImageCreateInfo :: Ptr Void -> [VkImageCreateFlagBits] -> VkImageType -> VkFormat -> VkExtent3D -> Word32 -> Word32 ->
    VkSampleCountFlagBits -> VkImageTiling -> [VkImageUsageFlagBits] -> VkSharingMode -> Word32 -> [Word32] -> VkImageLayout ->
    IO VkImageCreateInfo
createVkImageCreateInfo v cFlags t f e m a s ti uFlags mo iC indices l = allocaArray i $ \p -> do
    pokeArray p indices
    return $ VkImageCreateInfo structureTypeImageCreateInfo v c t f e m a s ti u mo iC p l
        where
            i = cast iC
            c = VkImageCreateFlags $ vkBits unVkImageCreateFlagBits cFlags
            u = VkImageUsageFlags $ vkBits unVkImageUsageFlagBits uFlags

createVkImageSubresource :: [VkImageAspectFlagBits] -> Word32 -> Word32 -> IO VkImageSubresource
createVkImageSubresource b m a = return $ VkImageSubresource f m a
    where
        f = VkImageAspectFlags $ vkBits unVkImageAspectFlagBits b

vkBindImageMemory :: VkDevice  -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult
vkBindImageMemory = c_vkBindImageMemory

vkCreateImage :: VkDevice -> VkImageCreateInfo -> IO VkImage
vkCreateImage device info = alloca $ \pInfo ->
    alloca $ \pImage -> do
        poke pInfo info
        _ <- c_vkCreateImage device pInfo nullPtr pImage
        peek pImage

vkCreateImageView :: VkDevice -> VkImageViewCreateInfo -> IO VkImageView
vkCreateImageView d i = alloca $ \pIVCI ->
    alloca $ \pIV -> do
        poke pIVCI i
        _ <- c_vkCreateImageView d pIVCI nullPtr pIV
        peek pIV

vkDestroyImage :: VkDevice -> VkImage -> IO ()
vkDestroyImage d i = c_vkDestroyImage d i nullPtr

vkGetImageMemoryRequirements :: VkDevice -> VkImage -> IO VkMemoryRequirements
vkGetImageMemoryRequirements d i = alloca $ \p -> do
    c_vkGetImageMemoryRequirements d i p
    peek p

vkGetImageSubresourceLayout :: VkDevice -> VkImage -> VkImageSubresource -> IO VkSubresourceLayout
vkGetImageSubresourceLayout d i s = alloca $ \pSub ->
    alloca $ \pLay -> do
        poke pSub s
        c_vkGetImageSubresourceLayout d i pSub pLay
        peek pLay
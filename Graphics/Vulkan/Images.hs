{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Images (createVkImageCreateInfo, createVkImageFormatListCreateInfo, createVkImageSubresource, createVkImageSubresourceLayers, createVkImageSubresourceRange, vkBindImageMemory, vkCreateImage, vkCreateImageView, vkDestroyImage, vkDestroyImageView, vkGetImageMemoryRequirements, vkGetImageSubresourceLayout) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type ArrayLayer             = Word32
type ArrayLayers            = Word32
type BaseArrayLayer         = Word32
type BaseMipLevel           = Word32
type LayerCount             = Word32
type LevelCount             = Word32
type MipLevel               = Word32
type MipLevels              = Word32
type QueueFamilyIndexCount  = Word32
type QueueFamilyIndices     = [Word32]
type ViewFormatCount        = Word32


foreign import ccall unsafe "vkBindImageMemory"
    c_vkBindImageMemory :: VkDevice  -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

foreign import ccall unsafe "vkCreateImage"
    c_vkCreateImage :: VkDevice -> Ptr VkImageCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult

foreign import ccall unsafe "vkCreateImageView"
    c_vkCreateImageView :: VkDevice -> Ptr VkImageViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult

foreign import ccall unsafe "vkDestroyImage"
    c_vkDestroyImage :: VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkDestroyImageView"
    c_vkDestroyImageView :: VkDevice -> VkImageView -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkGetImageMemoryRequirements"
    c_vkGetImageMemoryRequirements :: VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()

foreign import ccall unsafe "vkGetImageSubresourceLayout"
    c_vkGetImageSubresourceLayout :: VkDevice -> VkImage -> Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()

-- Note: ImageLayout needs to be imageLayoutUndefined or imageLayoutPreinitialized
createVkImageCreateInfo :: Next -> [VkImageCreateFlagBits] -> VkImageType -> VkFormat -> VkExtent3D -> MipLevels -> ArrayLayers ->
    VkSampleCountFlagBits -> VkImageTiling -> [VkImageUsageFlagBits] -> VkSharingMode -> QueueFamilyIndexCount -> QueueFamilyIndices ->
    VkImageLayout -> IO VkImageCreateInfo
createVkImageCreateInfo v cFlags t f e m a s ti uFlags mo iC indices l = allocaArray i $ \p -> do
    pokeArray p indices
    return $ VkImageCreateInfo structureTypeImageCreateInfo v c t f e m a s ti u mo iC p l
        where
            i = cast iC
            c = VkImageCreateFlags $ vkBits unVkImageCreateFlagBits cFlags
            u = VkImageUsageFlags $ vkBits unVkImageUsageFlagBits uFlags

createVkImageFormatListCreateInfo :: Next -> ViewFormatCount -> [VkFormat] -> IO VkImageFormatListCreateInfo
createVkImageFormatListCreateInfo v vFC f = allocaArray i $ \p -> do
    pokeArray p f
    return $ VkImageFormatListCreateInfo structureTypeImageFormatListCreateInfo v vFC p
    where
        i = cast vFC

createVkImageSubresource :: [VkImageAspectFlagBits] -> MipLevel -> ArrayLayer -> IO VkImageSubresource
createVkImageSubresource b m a = return $ VkImageSubresource f m a
    where
        f = VkImageAspectFlags $ vkBits unVkImageAspectFlagBits b

createVkImageSubresourceLayers :: [VkImageAspectFlagBits] -> MipLevel -> BaseArrayLayer -> LayerCount -> VkImageSubresourceLayers
createVkImageSubresourceLayers b = VkImageSubresourceLayers f
    where
        f = VkImageAspectFlags $ vkBits unVkImageAspectFlagBits b

createVkImageSubresourceRange :: [VkImageAspectFlagBits] -> BaseMipLevel -> LevelCount -> BaseArrayLayer -> LayerCount ->
    VkImageSubresourceRange
createVkImageSubresourceRange iAFB = VkImageSubresourceRange iAF
    where
        iAF = VkImageAspectFlags $ vkBits unVkImageAspectFlagBits iAFB

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

vkDestroyImageView :: VkDevice -> VkImageView -> IO ()
vkDestroyImageView d v = c_vkDestroyImageView d v nullPtr

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
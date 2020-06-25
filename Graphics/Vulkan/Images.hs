{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Images (vkCreateImageInfo) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Note: ImageLayout needs to be imageLayoutUndefined or imageLayoutPreinitialized
vkCreateImageInfo :: Ptr Void -> [VkImageCreateFlagBits] -> VkImageType -> VkFormat -> VkExtent3D ->
    Word32 -> Word32 -> VkSampleCountFlagBits -> VkImageTiling -> [VkImageUsageFlagBits] ->
    VkSharingMode -> Word32 -> [Word32] -> VkImageLayout -> IO VkImageCreateInfo
vkCreateImageInfo v cFlags t f e m a s ti uFlags mo iC indices l = allocaArray i $ \p -> do
    pokeArray p indices
    return $ VkImageCreateInfo structureTypeImageCreateInfo v c t f e m a s ti u mo iC p l
        where
            i = cast iC
            c = VkImageCreateFlags $ vkBits unVkImageCreateFlagBits cFlags
            u = VkImageUsageFlags $ vkBits unVkImageUsageFlagBits uFlags
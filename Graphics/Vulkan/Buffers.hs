{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Buffers (vkCreateBufferInfo) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


vkCreateBufferInfo :: Ptr Void -> VkBufferCreateFlags -> VkDeviceSize -> [VkBufferUsageFlagBits] -> VkSharingMode ->
    Word32 -> [Word32] -> IO VkBufferCreateInfo
vkCreateBufferInfo v bCF dS bUFB sM iC ind = allocaArray i $ \p -> do
    pokeArray p ind
    return $ VkBufferCreateInfo structureTypeBufferCreateInfo v bCF dS u sM iC p
        where
            i = cast iC
            u = VkBufferUsageFlags $ vkBits unVkBufferUsageFlagBits bUFB
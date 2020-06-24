{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Buffers (vkCreateBuffer, vkCreateBufferInfo, vkGetBufferMemoryRequirements) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkCreateBuffer"
    c_vkCreateBuffer :: VkDevice -> Ptr VkBufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBuffer -> IO VkResult

foreign import ccall unsafe "vkGetBufferMemoryRequirements"
    c_vkGetBufferMemoryRequirements :: VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()

vkCreateBuffer :: VkDevice -> VkBufferCreateInfo -> IO VkBuffer
vkCreateBuffer device info = alloca $ \pInfo ->
    alloca $ \pBuffer -> do
        poke pInfo info
        _ <- c_vkCreateBuffer device pInfo nullPtr pBuffer
        peek pBuffer

vkCreateBufferInfo :: Ptr Void -> VkBufferCreateFlags -> VkDeviceSize -> [VkBufferUsageFlagBits] -> VkSharingMode ->
    Word32 -> [Word32] -> IO VkBufferCreateInfo
vkCreateBufferInfo v bCF dS bUFB sM iC ind = allocaArray i $ \p -> do
    pokeArray p ind
    return $ VkBufferCreateInfo structureTypeBufferCreateInfo v bCF dS u sM iC p
        where
            i = cast iC
            u = VkBufferUsageFlags $ vkBits unVkBufferUsageFlagBits bUFB

vkGetBufferMemoryRequirements :: VkDevice -> VkBuffer -> IO VkMemoryRequirements
vkGetBufferMemoryRequirements d b = alloca $ \p -> do
    c_vkGetBufferMemoryRequirements d b p
    peek p
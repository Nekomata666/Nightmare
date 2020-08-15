{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Buffers (createVkBufferInfo, vkBindBufferMemory, vkCreateBuffer, vkDestroyBuffer, vkGetBufferMemoryRequirements) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type MemoryOffset           = VkDeviceSize
type QueueFamilyIndexCount  = Word32
type QueueFamilyIndices     = Word32

foreign import ccall unsafe "vkBindBufferMemory"
    c_vkBindBufferMemory :: VkDevice  -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

foreign import ccall unsafe "vkCreateBuffer"
    c_vkCreateBuffer :: VkDevice -> Ptr VkBufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBuffer -> IO VkResult

foreign import ccall unsafe "vkDestroyBuffer"
    c_vkDestroyBuffer :: VkDevice -> VkBuffer -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkGetBufferMemoryRequirements"
    c_vkGetBufferMemoryRequirements :: VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()

createVkBufferInfo :: Next -> VkBufferCreateFlags -> VkDeviceSize -> [VkBufferUsageFlagBits] -> VkSharingMode ->
    QueueFamilyIndexCount -> [QueueFamilyIndices] -> IO VkBufferCreateInfo
createVkBufferInfo v bCF dS bUFB sM iC ind = allocaArray i $ \p -> do
    pokeArray p ind
    return $ VkBufferCreateInfo structureTypeBufferCreateInfo v bCF dS u sM iC p
        where
            i = cast iC
            u = VkBufferUsageFlags $ vkBits unVkBufferUsageFlagBits bUFB

vkBindBufferMemory :: VkDevice  -> VkBuffer -> VkDeviceMemory -> MemoryOffset -> IO VkResult
vkBindBufferMemory = c_vkBindBufferMemory

vkCreateBuffer :: VkDevice -> VkBufferCreateInfo -> IO VkBuffer
vkCreateBuffer device info = alloca $ \pInfo ->
    alloca $ \pBuffer -> do
        poke pInfo info
        _ <- c_vkCreateBuffer device pInfo nullPtr pBuffer
        peek pBuffer

vkDestroyBuffer :: VkDevice -> VkBuffer -> IO ()
vkDestroyBuffer d b = c_vkDestroyBuffer d b nullPtr

vkGetBufferMemoryRequirements :: VkDevice -> VkBuffer -> IO VkMemoryRequirements
vkGetBufferMemoryRequirements d b = alloca $ \p -> do
    c_vkGetBufferMemoryRequirements d b p
    peek p
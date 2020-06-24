{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Memory (vkAllocateMemory, vkCreateMemoryAllocateInfo, vkMapMemory) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities (cast)

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkAllocateMemory"
    c_vkAllocateMemory :: VkDevice -> Ptr VkMemoryAllocateInfo -> Ptr VkAllocationCallbacks -> Ptr
        VkDeviceMemory -> IO VkResult

foreign import ccall unsafe "vkMapMemory"
    c_vkMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags
        -> Ptr (Ptr Void) -> IO VkResult

vkAllocateMemory :: VkDevice -> VkMemoryAllocateInfo -> IO VkDeviceMemory
vkAllocateMemory d i = alloca $ \pInfo ->
    alloca $ \pMem -> do
        poke pInfo i
        _ <- c_vkAllocateMemory d pInfo nullPtr pMem
        peek pMem

vkCreateMemoryAllocateInfo :: Ptr Void -> VkDeviceSize -> Word32 -> VkMemoryAllocateInfo
vkCreateMemoryAllocateInfo = VkMemoryAllocateInfo structureTypeMemoryAllocateInfo

vkMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> IO (Ptr Void)
vkMapMemory d dm o s f = alloca $ \p -> do
    _ <- c_vkMapMemory d dm o s f p
    peek p
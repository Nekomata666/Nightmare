{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Memory (vkAllocateMemory, vkCreateMemoryAllocateInfo) where


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

vkAllocateMemory :: VkDevice -> VkMemoryAllocateInfo -> IO VkDeviceMemory
vkAllocateMemory d i = alloca $ \pInfo ->
    alloca $ \pMem -> do
        poke pInfo i
        _ <- c_vkAllocateMemory d pInfo nullPtr pMem
        peek pMem

vkCreateMemoryAllocateInfo :: Ptr Void -> VkDeviceSize -> Word32 -> VkMemoryAllocateInfo
vkCreateMemoryAllocateInfo = VkMemoryAllocateInfo structureTypeMemoryAllocateInfo
{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Memory (vkAllocateMemory, vkCreateMemoryAllocateInfo, vkFreeMemory, vkMapMemory, vkUnmapMemory) where


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

foreign import ccall unsafe "vkFreeMemory"
    c_vkFreeMemory :: VkDevice -> VkDeviceMemory -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkMapMemory"
    c_vkMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags
        -> Ptr (Ptr Void) -> IO VkResult

foreign import ccall unsafe "vkUnmapMemory"
    c_vkUnmapMemory :: VkDevice -> VkDeviceMemory -> IO ()

vkAllocateMemory :: VkDevice -> VkMemoryAllocateInfo -> IO VkDeviceMemory
vkAllocateMemory d i = alloca $ \pInfo ->
    alloca $ \pMem -> do
        poke pInfo i
        _ <- c_vkAllocateMemory d pInfo nullPtr pMem
        peek pMem

vkCreateMemoryAllocateInfo :: Ptr Void -> VkDeviceSize -> Word32 -> VkMemoryAllocateInfo
vkCreateMemoryAllocateInfo = VkMemoryAllocateInfo structureTypeMemoryAllocateInfo

vkFreeMemory :: VkDevice -> VkDeviceMemory -> IO ()
vkFreeMemory d m = c_vkFreeMemory d m nullPtr

vkMapMemory :: VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> IO (Ptr Void)
vkMapMemory d dm o s f = alloca $ \p -> do
    _ <- c_vkMapMemory d dm o s f p
    peek p

vkUnmapMemory :: VkDevice -> VkDeviceMemory -> IO ()
vkUnmapMemory = c_vkUnmapMemory
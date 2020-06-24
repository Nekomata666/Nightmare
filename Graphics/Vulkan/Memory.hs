{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Memory (vkCreateMemoryAllocateInfo) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities (cast)

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


vkCreateMemoryAllocateInfo :: Ptr Void -> VkDeviceSize -> Word32 -> VkMemoryAllocateInfo
vkCreateMemoryAllocateInfo = VkMemoryAllocateInfo structureTypeMemoryAllocateInfo
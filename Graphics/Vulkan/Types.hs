{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Types where


import Data.Word (Word32)

import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, FunPtr)

import Graphics.Vulkan.Enumerations (VkInternalAllocationType, VkSystemAllocationScope)


-- Vulkan Type aliases.
type VkFlags    = Word32
type VkHandle   = Word64


-- Vulkan Handles
newtype VkInstance = VkInstance { unVkInstance :: VkHandle }


-- Vulkan function pointers.
type PFN_vkVoidFunction = FunPtr (IO ())
type PFN_vkAllocationFunction = FunPtr (Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkReallocationFunction = FunPtr (Ptr Void -> Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())
type PFN_vkInternalAllocationNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
type PFN_vkInternalFreeNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Types where


import Data.Void (Void)
import Data.Word (Word32, Word64)

import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable

import Graphics.Vulkan.Enumerations (VkInternalAllocationType, VkSystemAllocationScope)


-- Vulkan Type aliases.
type VkFlags    = Word32
type VkHandle   = Word64


-- Vulkan Handles
newtype VkInstance = VkInstance { unVkInstance :: VkHandle }
newtype VkPhysicalDevice = VkPhysicalDevice { unVkPhysicalDevice :: VkHandle }


-- Vulkan function pointers.
type PFN_vkVoidFunction = FunPtr (IO ())
type PFN_vkAllocationFunction = FunPtr (Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkReallocationFunction = FunPtr (Ptr Void -> Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())
type PFN_vkInternalAllocationNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
type PFN_vkInternalFreeNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

instance Storable VkInstance where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkInstance v)
    poke p (VkInstance v) = pokeByteOff p 0 v

instance Storable VkPhysicalDevice where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPhysicalDevice v)
    poke p (VkPhysicalDevice v) = pokeByteOff p 0 v
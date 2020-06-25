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

-- Vulkan Types
newtype VkBool  = VkBool { unVkBool :: Word32 }

-- Vulkan Flags
newtype VkBufferCreateFlags = VkBufferCreateFlags { unVkBufferCreateFlags :: VkFlags }
newtype VkBufferUsageFlags = VkBufferUsageFlags { unVkBufferUsageFlags :: VkFlags }
newtype VkDeviceCreateFlags = VkDeviceCreateFlags { unVkDeviceCreateFlags :: VkFlags }
newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags { unVkDeviceQueueCreateFlags :: VkFlags }
newtype VkImageCreateFlags = VkImageCreateFlags { unVkImageCreateFlags :: VkFlags }
newtype VkImageUsageFlags = VkImageUsageFlags { unVkImageUsageFlags :: VkFlags }
newtype VkMemoryMapFlags = VkMemoryMapFlags { unVkMemoryMapFlags :: VkFlags }


-- Vulkan Handles
newtype VkBuffer = VkBuffer { unVkBuffer :: VkHandle }
newtype VkDevice = VkDevice { unVkDevice :: VkHandle }
newtype VkDeviceMemory = VkDeviceMemory { unVkDeviceMemory :: VkHandle }
newtype VkDeviceSize = VkDeviceSize { unVkDeviceSize :: VkHandle }
newtype VkInstance = VkInstance { unVkInstance :: VkHandle }
newtype VkPhysicalDevice = VkPhysicalDevice { unVkPhysicalDevice :: VkHandle }


-- Vulkan function pointers.
type PFN_vkVoidFunction = FunPtr (IO ())
type PFN_vkAllocationFunction = FunPtr (Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkReallocationFunction = FunPtr (Ptr Void -> Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())
type PFN_vkInternalAllocationNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
type PFN_vkInternalFreeNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

-- Storable instances for Vulkan types.
instance Storable VkBool where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBool v)
    poke p (VkBool v) = pokeByteOff p 0 v

-- Storable instances for Vulkan flags.
instance Storable VkBufferCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferCreateFlags v)
    poke p (VkBufferCreateFlags v) = pokeByteOff p 0 v

instance Storable VkBufferUsageFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferUsageFlags v)
    poke p (VkBufferUsageFlags v) = pokeByteOff p 0 v

instance Storable VkDeviceCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceCreateFlags v)
    poke p (VkDeviceCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDeviceQueueCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceQueueCreateFlags v)
    poke p (VkDeviceQueueCreateFlags v) = pokeByteOff p 0 v

instance Storable VkImageCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageCreateFlags v)
    poke p (VkImageCreateFlags v) = pokeByteOff p 0 v

instance Storable VkImageUsageFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageUsageFlags v)
    poke p (VkImageUsageFlags v) = pokeByteOff p 0 v

instance Storable VkMemoryMapFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkMemoryMapFlags v)
    poke p (VkMemoryMapFlags v) = pokeByteOff p 0 v

-- Storable instances for Vulkan handles.
instance Storable VkBuffer where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkBuffer v)
    poke p (VkBuffer v) = pokeByteOff p 0 v

instance Storable VkDevice where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDevice v)
    poke p (VkDevice v) = pokeByteOff p 0 v

instance Storable VkDeviceMemory where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceMemory v)
    poke p (VkDeviceMemory v) = pokeByteOff p 0 v

instance Storable VkDeviceSize where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceSize v)
    poke p (VkDeviceSize v) = pokeByteOff p 0 v

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
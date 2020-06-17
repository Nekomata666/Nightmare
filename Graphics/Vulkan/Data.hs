{-# LANGUAGE DuplicateRecordFields, Safe #-}

module Graphics.Vulkan.Data where


import Data.Void (Void)
import Data.Word (Word8, Word32)

import Foreign
import Foreign.C.String

import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


data VkAllocationCallbacks = VkAllocationCallbacks{
    userData                :: Ptr Void,
    pfnAllocation           :: PFN_vkAllocationFunction,
    pfnReallocation         :: PFN_vkReallocationFunction,
    pfnFree                 :: PFN_vkFreeFunction,
    pfnInternalAllocation   :: PFN_vkInternalAllocationNotification,
    pfnInternalFree         :: PFN_vkInternalFreeNotification
}
data VkApplicationInfo = VkApplicationInfo{
    sType               :: VkStructureType,
    next                :: Ptr Void,
    applicationName     :: CString,
    applicationVersion  :: Word32,
    engineName          :: CString,
    engineVersion       :: Word32,
    apiVersion          :: Word32
}
data VkInstanceCreateInfo = VkInstanceCreateInfo{
    sType                   :: VkStructureType,
    next                    :: Ptr Void,
    flags                   :: VkFlags,
    applicationInfo         :: Ptr VkApplicationInfo,
    enabledLayerCount       :: Word32,
    enabledLayerNames       :: Ptr CString,
    enabledExtensionCount   :: Word32,
    enabledExtensionNames   :: Ptr CString
}

-- Storable instances
instance Storable VkAllocationCallbacks where
    sizeOf _ = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        return (VkAllocationCallbacks v1 v2 v3 v4 v5 v6)
    poke p (VkAllocationCallbacks v1 v2 v3 v4 v5 v6) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6

instance Storable VkApplicationInfo where
    sizeOf _ = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 44
        return (VkApplicationInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkApplicationInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 44 v7

instance Storable VkInstanceCreateInfo where
    sizeOf _ = 64
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 48
        v8 <- peekByteOff p 56
        return (VkInstanceCreateInfo v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkInstanceCreateInfo v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 48 v7
        pokeByteOff p 56 v8

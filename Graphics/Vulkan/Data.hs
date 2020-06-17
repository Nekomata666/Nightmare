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

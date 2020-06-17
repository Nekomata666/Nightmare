{-# LANGUAGE DuplicateRecordFields, Safe #-}

module Graphics.Vulkan.Data where


import Data.Void (Void)
import Data.Word (Word8, Word32)

import Foreign
import Foreign.C.String

import Graphics.Vulkan.Types


data VkApplicationInfo = VkApplicationInfo{
    sType               :: VkStructureType,
    next                :: Ptr Void,
    applicationName     :: CString,
    applicationVersion  :: Word32,
    engineName          :: CString,
    engineVersion       :: Word32,
    apiVersion          :: Word32
}

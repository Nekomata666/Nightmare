{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Constants where


import Data.Word (Word32, Word64)

import Graphics.Vulkan.Types (VkDeviceSize(..), VkBool(..))



vkFalse :: VkBool
vkFalse = VkBool 0
vkTrue  :: VkBool
vkTrue  = VkBool 1

wholeSize :: VkDeviceSize
wholeSize = VkDeviceSize 18446744073709551615
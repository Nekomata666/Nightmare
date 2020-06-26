{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Command (createClearColorValue) where


import Data.Maybe (Maybe)
import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


createClearColorValue :: [Word32] -> IO VkClearColorValue
createClearColorValue v = allocaArray 4 $ \p -> do
    pokeArray p v
    return $ VkClearColorValue p
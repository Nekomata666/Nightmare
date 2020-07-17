{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Framebuffer () where


import Data.Maybe   (Maybe)
import Data.Void    (Void)
import Data.Word    (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types
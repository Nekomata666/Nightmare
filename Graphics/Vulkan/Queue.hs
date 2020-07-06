{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Queue () where


import Control.DeepSeq
import Control.Exception.Base

import Data.Maybe
import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types
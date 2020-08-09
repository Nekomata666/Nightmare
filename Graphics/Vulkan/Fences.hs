{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Fences (vkCreateFence) where


import Data.Word (Word32, Word64)

import Foreign

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkCreateFence"
    c_vkCreateFence :: VkDevice -> Ptr VkFenceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult

vkCreateFence :: VkDevice -> VkFenceCreateInfo -> IO VkFence
vkCreateFence d fCI = alloca $ \pFCI ->
    alloca $ \pF -> do
        poke pFCI fCI
        _ <- c_vkCreateFence d pFCI nullPtr pF
        peek pF
{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Fences (vkCreateFence, vkWaitForFences) where


import Data.Word (Word32, Word64)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type FenceCount = Word32
type TimeOut    = Word64

foreign import ccall unsafe "vkCreateFence"
    c_vkCreateFence :: VkDevice -> Ptr VkFenceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult

foreign import ccall unsafe "vkWaitForFences"
    c_vkWaitForFences :: VkDevice -> Word32 -> Ptr VkFence -> VkBool -> Word64 -> IO VkResult

vkCreateFence :: VkDevice -> VkFenceCreateInfo -> IO VkFence
vkCreateFence d fCI = alloca $ \pFCI ->
    alloca $ \pF -> do
        poke pFCI fCI
        _ <- c_vkCreateFence d pFCI nullPtr pF
        peek pF

vkWaitForFences :: VkDevice -> FenceCount -> [VkFence] -> VkBool -> TimeOut -> IO VkResult
vkWaitForFences d fC f b tO = allocaArray i $ \p -> do
    pokeArray p f
    c_vkWaitForFences d fC p b tO
    where
        i = cast fC
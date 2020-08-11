{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Fences (vkCreateFence, vkDestroyFence, vkResetFences, vkWaitForFences) where


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

foreign import ccall unsafe "vkDestroyFence"
    c_vkDestroyFence :: VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkResetFences"
    c_vkResetFences :: VkDevice -> Word32 -> Ptr VkFence -> IO VkResult

foreign import ccall unsafe "vkWaitForFences"
    c_vkWaitForFences :: VkDevice -> Word32 -> Ptr VkFence -> VkBool -> Word64 -> IO VkResult

vkCreateFence :: VkDevice -> VkFenceCreateInfo -> IO VkFence
vkCreateFence d fCI = alloca $ \pFCI ->
    alloca $ \pF -> do
        poke pFCI fCI
        _ <- c_vkCreateFence d pFCI nullPtr pF
        peek pF

vkDestroyFence :: VkDevice -> VkFence -> IO ()
vkDestroyFence d f = c_vkDestroyFence d f nullPtr

vkResetFences :: VkDevice -> FenceCount -> [VkFence] -> IO VkResult
vkResetFences d fC f = allocaArray i $ \p -> do
    pokeArray p f
    c_vkResetFences d fC p
    where
        i = cast fC

vkWaitForFences :: VkDevice -> FenceCount -> [VkFence] -> VkBool -> TimeOut -> IO VkResult
vkWaitForFences d fC f b tO = allocaArray i $ \p -> do
    pokeArray p f
    c_vkWaitForFences d fC p b tO
    where
        i = cast fC
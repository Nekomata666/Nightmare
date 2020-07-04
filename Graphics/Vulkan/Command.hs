{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Command (createVkClearColorValue, createVkCommandBufferAllocateInfo, createVkCommandPoolInfo, vkCreateCommandPool) where


import Data.Maybe   (Maybe)
import Data.Void    (Void)
import Data.Word    (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkCreateCommandPool"
    c_vkCreateCommandPool :: VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool ->
        IO VkResult


createVkClearColorValue :: [Word32] -> IO VkClearColorValue
createVkClearColorValue v = allocaArray 4 $ \p -> do
    pokeArray p v
    return $ VkClearColorValue p

createVkCommandBufferAllocateInfo :: Ptr Void -> VkCommandPool -> VkCommandBufferLevel -> Word32 -> VkCommandBufferAllocateInfo
createVkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo structureTypeCommandBufferAllocateInfo

createVkCommandPoolInfo :: Ptr Void -> VkCommandPoolCreateFlags -> Word32 -> VkCommandPoolCreateInfo
createVkCommandPoolInfo = VkCommandPoolCreateInfo structureTypeCommandPoolCreateInfo

vkCreateCommandPool :: VkDevice -> VkCommandPoolCreateInfo -> IO VkCommandPool
vkCreateCommandPool d info = alloca $ \pInfo ->
    alloca $ \pPool -> do
        poke pInfo info
        _ <- c_vkCreateCommandPool d pInfo nullPtr pPool
        peek pPool
{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Command (createVkClearColorValue, createVkCommandBufferAllocateInfo, createVkCommandBufferBeginInfo, createVkCommandPoolInfo, vkAllocateCommandBuffers, vkCreateCommandPool) where


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

foreign import ccall unsafe "vkAllocateCommandBuffers"
    c_vkAllocateCommandBuffers :: VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult


createVkClearColorValue :: [Word32] -> IO VkClearColorValue
createVkClearColorValue v = allocaArray 4 $ \p -> do
    pokeArray p v
    return $ VkClearColorValue p

createVkCommandBufferAllocateInfo :: Ptr Void -> VkCommandPool -> VkCommandBufferLevel -> Word32 -> VkCommandBufferAllocateInfo
createVkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo structureTypeCommandBufferAllocateInfo

createVkCommandBufferBeginInfo :: Ptr Void -> VkCommandBufferUsageFlags -> Maybe VkCommandBufferInheritanceInfo -> IO VkCommandBufferBeginInfo
createVkCommandBufferBeginInfo v f i = do
    p <- fromMaybeIO i
    return $ VkCommandBufferBeginInfo structureTypeCommandBufferBeginInfo v f p

createVkCommandPoolInfo :: Ptr Void -> VkCommandPoolCreateFlags -> Word32 -> VkCommandPoolCreateInfo
createVkCommandPoolInfo = VkCommandPoolCreateInfo structureTypeCommandPoolCreateInfo

vkAllocateCommandBuffers :: VkDevice -> VkCommandBufferAllocateInfo -> IO [VkCommandBuffer]
vkAllocateCommandBuffers d info@(VkCommandBufferAllocateInfo _ _ _ _ c) = alloca $ \pInfo ->
    allocaArray i $ \pBuffy -> do
        poke pInfo info
        _ <- c_vkAllocateCommandBuffers d pInfo pBuffy
        peekArray i pBuffy
        where
            i = cast c

vkCreateCommandPool :: VkDevice -> VkCommandPoolCreateInfo -> IO VkCommandPool
vkCreateCommandPool d info = alloca $ \pInfo ->
    alloca $ \pPool -> do
        poke pInfo info
        _ <- c_vkCreateCommandPool d pInfo nullPtr pPool
        peek pPool
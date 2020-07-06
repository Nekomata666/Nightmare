{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Command (createVkClearColorValue, createVkCommandBufferAllocateInfo, createVkCommandBufferBeginInfo, createVkCommandPoolInfo, createVkImageSubresourceRange, vkAllocateCommandBuffers, vkBeginCommandBuffer, vkCmdClearColorImage, vkCmdFillBuffer, vkCreateCommandPool) where


import Data.Maybe   (Maybe)
import Data.Void    (Void)
import Data.Word    (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type BaseArrayLayer             = Word32
type BaseMipLevel               = Word32
type Data                       = Word32
type LayerCount                 = Word32
type LevelCount                 = Word32
type Offset                     = VkDeviceSize
type RangeCount                 = Word32
type Size                       = VkDeviceSize

foreign import ccall unsafe "vkAllocateCommandBuffers"
    c_vkAllocateCommandBuffers :: VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult

foreign import ccall unsafe "vkBeginCommandBuffer"
    c_vkBeginCommandBuffer :: VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult

foreign import ccall unsafe "vkCmdClearColorImage"
    c_vkCmdClearColorImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearColorValue -> Word32 ->
        Ptr VkImageSubresourceRange -> IO ()

foreign import ccall unsafe "vkCmdFillBuffer"
    c_vkCmdFillBuffer :: VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()

foreign import ccall unsafe "vkCreateCommandPool"
    c_vkCreateCommandPool :: VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool ->
        IO VkResult


createVkClearColorValue :: [Word32] -> IO VkClearColorValue
createVkClearColorValue v = allocaArray 4 $ \p -> do
    pokeArray p v
    return $ VkClearColorValue p

createVkCommandBufferAllocateInfo :: Ptr Void -> VkCommandPool -> VkCommandBufferLevel -> Word32 -> VkCommandBufferAllocateInfo
createVkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo structureTypeCommandBufferAllocateInfo

createVkCommandBufferBeginInfo :: Ptr Void -> VkCommandBufferUsageFlags -> Maybe VkCommandBufferInheritanceInfo ->
    IO VkCommandBufferBeginInfo
createVkCommandBufferBeginInfo v f i = do
    p <- fromMaybeIO i
    return $ VkCommandBufferBeginInfo structureTypeCommandBufferBeginInfo v f p

createVkCommandPoolInfo :: Ptr Void -> VkCommandPoolCreateFlags -> Word32 -> VkCommandPoolCreateInfo
createVkCommandPoolInfo = VkCommandPoolCreateInfo structureTypeCommandPoolCreateInfo

createVkImageSubresourceRange :: [VkImageAspectFlagBits] -> BaseMipLevel -> LevelCount -> BaseArrayLayer -> LayerCount ->
    VkImageSubresourceRange
createVkImageSubresourceRange iAFB = VkImageSubresourceRange iAF
    where
        iAF = VkImageAspectFlags $ vkBits unVkImageAspectFlagBits iAFB

vkAllocateCommandBuffers :: VkDevice -> VkCommandBufferAllocateInfo -> IO [VkCommandBuffer]
vkAllocateCommandBuffers d info@(VkCommandBufferAllocateInfo _ _ _ _ c) = alloca $ \pInfo ->
    allocaArray i $ \pBuffy -> do
        poke pInfo info
        _ <- c_vkAllocateCommandBuffers d pInfo pBuffy
        peekArray i pBuffy
        where
            i = cast c

vkBeginCommandBuffer :: VkCommandBuffer -> VkCommandBufferBeginInfo -> IO VkResult
vkBeginCommandBuffer b i = alloca $ \p -> do
    poke p i
    c_vkBeginCommandBuffer b p

vkCmdClearColorImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> VkClearColorValue -> RangeCount ->
    [VkImageSubresourceRange] -> IO ()
vkCmdClearColorImage cB vkI iL cCV rC iSR = alloca $ \pCCV ->
    allocaArray i $ \pISR -> do
        poke pCCV cCV
        pokeArray pISR iSR
        c_vkCmdClearColorImage cB vkI iL pCCV rC pISR
        where
            i = cast rC

-- Note: Offset and Size need to be in multiples of 4.
vkCmdFillBuffer :: VkCommandBuffer -> VkBuffer -> Offset -> Size -> Data -> IO ()
vkCmdFillBuffer = c_vkCmdFillBuffer

vkCreateCommandPool :: VkDevice -> VkCommandPoolCreateInfo -> IO VkCommandPool
vkCreateCommandPool d info = alloca $ \pInfo ->
    alloca $ \pPool -> do
        poke pInfo info
        _ <- c_vkCreateCommandPool d pInfo nullPtr pPool
        peek pPool
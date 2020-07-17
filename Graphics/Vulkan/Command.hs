{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Command (createVkClearColorValue, createVkCommandBufferAllocateInfo, createVkCommandBufferBeginInfo, createVkCommandPoolInfo, createVkImageSubresourceRange, createVkRenderPassBeginInfo, vkAllocateCommandBuffers, vkBeginCommandBuffer, vkCmdBeginRenderPass, vkCmdBindDescriptorSets, vkCmdBindPipeline, vkCmdClearColorImage, vkCmdFillBuffer, vkCmdPushConstants, vkCreateCommandPool, vkDestroyCommandPool, vkEndCommandBuffer) where


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
type ClearValueCount            = Word32
type Data                       = Word32
type DescriptorSetCount         = Word32
type DynamicOffsetCount         = Word32
type DynamicOffsets             = Word32
type FirstSet                   = Word32
type LayerCount                 = Word32
type LevelCount                 = Word32
type Offset                     = VkDeviceSize
type PushOffset                 = Word32
type PushSize                   = Word32
type QueueFamilyIndex           = Word32
type RangeCount                 = Word32
type Size                       = VkDeviceSize

foreign import ccall unsafe "vkAllocateCommandBuffers"
    c_vkAllocateCommandBuffers :: VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult

foreign import ccall unsafe "vkBeginCommandBuffer"
    c_vkBeginCommandBuffer :: VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult

foreign import ccall unsafe "vkCmdBeginRenderPass"
    c_vkCmdBeginRenderPass :: VkCommandBuffer -> Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()

foreign import ccall unsafe "vkCmdBindDescriptorSets"
    c_vkCmdBindDescriptorSets :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> Word32 ->
        Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "vkCmdBindPipeline"
    c_vkCmdBindPipeline :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()

foreign import ccall unsafe "vkCmdClearColorImage"
    c_vkCmdClearColorImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearColorValue -> Word32 ->
        Ptr VkImageSubresourceRange -> IO ()

foreign import ccall unsafe "vkCmdFillBuffer"
    c_vkCmdFillBuffer :: VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()

foreign import ccall unsafe "vkCmdPushConstants"
    c_vkCmdPushConstants :: VkCommandBuffer -> VkPipelineLayout -> VkShaderStageFlags -> Word32 -> Word32 -> Ptr Void -> IO ()

foreign import ccall unsafe "vkCreateCommandPool"
    c_vkCreateCommandPool :: VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool ->
        IO VkResult

foreign import ccall unsafe "vkDestroyCommandPool"
    c_vkDestroyCommandPool :: VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkEndCommandBuffer"
    c_vkEndCommandBuffer :: VkCommandBuffer -> IO VkResult


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

createVkCommandPoolInfo :: Ptr Void -> VkCommandPoolCreateFlags -> QueueFamilyIndex -> VkCommandPoolCreateInfo
createVkCommandPoolInfo = VkCommandPoolCreateInfo structureTypeCommandPoolCreateInfo

createVkImageSubresourceRange :: [VkImageAspectFlagBits] -> BaseMipLevel -> LevelCount -> BaseArrayLayer -> LayerCount ->
    VkImageSubresourceRange
createVkImageSubresourceRange iAFB = VkImageSubresourceRange iAF
    where
        iAF = VkImageAspectFlags $ vkBits unVkImageAspectFlagBits iAFB

createVkRenderPassBeginInfo :: Ptr Void -> VkRenderPass -> VkFramebuffer -> VkRect2D -> ClearValueCount -> [VkClearValue] -> IO VkRenderPassBeginInfo
createVkRenderPassBeginInfo v rP fB r2D cVC cV = allocaArray i $ \p -> do
    pokeArray p cV
    return $ VkRenderPassBeginInfo structureTypeRenderPassBeginInfo v rP fB r2D cVC p
    where
        i = cast cVC

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

vkCmdBeginRenderPass :: VkCommandBuffer -> VkRenderPassBeginInfo -> VkSubpassContents -> IO ()
vkCmdBeginRenderPass cB rPBI sC = alloca $ \p -> do
    poke p rPBI
    c_vkCmdBeginRenderPass cB p sC

vkCmdBindDescriptorSets :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> FirstSet -> DescriptorSetCount ->
    [VkDescriptorSet] -> DynamicOffsetCount -> Maybe [DynamicOffsets] -> IO ()
vkCmdBindDescriptorSets cB pBP pL fS dSC dS dOC dO = allocaArray i $ \pDS -> do
    pDO <- fromMaybeListIO dOC dO
    pokeArray pDS dS
    c_vkCmdBindDescriptorSets cB pBP pL fS dSC pDS dOC pDO
    where
        i = cast dSC

vkCmdBindPipeline :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()
vkCmdBindPipeline = c_vkCmdBindPipeline

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

vkCmdPushConstants :: Storable a => VkCommandBuffer -> VkPipelineLayout -> [VkShaderStageFlagBits] -> PushOffset -> PushSize ->
    a -> IO ()
vkCmdPushConstants cB pL sSFB o s v = alloca $ \p -> do
    poke p v
    c_vkCmdPushConstants cB pL sSF o s (castPtr p)
    where
        sSF = VkShaderStageFlags $ vkBits unVkShaderStageFlagBits sSFB

vkCreateCommandPool :: VkDevice -> VkCommandPoolCreateInfo -> IO VkCommandPool
vkCreateCommandPool d info = alloca $ \pInfo ->
    alloca $ \pPool -> do
        poke pInfo info
        _ <- c_vkCreateCommandPool d pInfo nullPtr pPool
        peek pPool

vkDestroyCommandPool :: VkDevice -> VkCommandPool -> IO ()
vkDestroyCommandPool d p = c_vkDestroyCommandPool d p nullPtr

vkEndCommandBuffer :: VkCommandBuffer -> IO VkResult
vkEndCommandBuffer = c_vkEndCommandBuffer
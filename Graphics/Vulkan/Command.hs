{-# LANGUAGE ForeignFunctionInterface, Safe #-}

-- Todo: Return when RayTracing is available.
module Graphics.Vulkan.Command (createVkCommandBufferAllocateInfo, createVkCommandBufferBeginInfo, createVkCommandPoolInfo, createVkRenderPassBeginInfo, vkAllocateCommandBuffers, vkBeginCommandBuffer, vkCmdBeginRenderPass, vkCmdBindDescriptorSets, vkCmdBindIndexBuffer, vkCmdBindPipeline, vkCmdBindVertexBuffers, vkCmdBlitImage, {-vkCmdBuildAccelerationStructureKHR,-} vkCmdClearColorImage, vkCmdCopyBuffer, vkCmdDispatch, vkCmdDraw, vkCmdDrawIndexed, vkCmdEndRenderPass,vkCmdExecuteCommands, vkCmdFillBuffer, vkCmdPipelineBarrier, vkCmdPushConstants, vkCmdResolveImage, vkCreateCommandPool, vkDestroyCommandPool, vkEndCommandBuffer, vkFreeCommandBuffers) where


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
type BindingCount               = Word32
type BufferMemoryBarrierCount   = Word32
type ClearValueCount            = Word32
type CommandBufferCount         = Word32
type Data                       = Word32
type DescriptorSetCount         = Word32
type DstBuffer                  = VkBuffer
type DstStageMask               = VkPipelineStageFlagBits
type DynamicOffsetCount         = Word32
type DynamicOffsets             = Word32
type FirstBinding               = Word32
type FirstIndex                 = Word32
type FirstInstance              = Word32
type FirstSet                   = Word32
type FirstVertex                = Word32
type GroupCountX                = Word32
type GroupCountY                = Word32
type GroupCountZ                = Word32
type ImageMemoryBarrierCount    = Word32
type IndexCount                 = Word32
type InfoCount                  = Word32
type InstanceCount              = Word32
type LayerCount                 = Word32
type MemoryBarrierCount         = Word32
type Offset                     = VkDeviceSize
type PushOffset                 = Word32
type PushSize                   = Word32
type QueueFamilyIndex           = Word32
type RangeCount                 = Word32
type RegionCount                = Word32
type Regions                    = [VkBufferCopy]
type Size                       = VkDeviceSize
type SrcBuffer                  = VkBuffer
type SrcStageMask               = VkPipelineStageFlagBits
type VertexCount                = Word32
type VertexOffset               = Int32


foreign import ccall unsafe "vkAllocateCommandBuffers"
    c_vkAllocateCommandBuffers :: VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult

foreign import ccall unsafe "vkBeginCommandBuffer"
    c_vkBeginCommandBuffer :: VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult

foreign import ccall unsafe "vkCmdBeginRenderPass"
    c_vkCmdBeginRenderPass :: VkCommandBuffer -> Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()

foreign import ccall unsafe "vkCmdBindDescriptorSets"
    c_vkCmdBindDescriptorSets :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> Word32 ->
        Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "vkCmdBindIndexBuffer"
    c_vkCmdBindIndexBuffer :: VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkIndexType -> IO ()

foreign import ccall unsafe "vkCmdBindPipeline"
    c_vkCmdBindPipeline :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()

foreign import ccall unsafe "vkCmdBindVertexBuffers"
    c_vkCmdBindVertexBuffers :: VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr VkDeviceSize -> IO ()

foreign import ccall unsafe "vkCmdBlitImage"
    c_vkCmdBlitImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageBlit ->
        VkFilter -> IO ()

foreign import ccall unsafe "vkCmdBuildAccelerationStructureKHR"
    c_vkCmdBuildAccelerationStructureKHR :: VkCommandBuffer -> Word32 -> Ptr VkAccelerationStructureBuildGeometryInfoKHR ->
        Ptr VkAccelerationStructureBuildOffsetInfoKHR -> IO ()

foreign import ccall unsafe "vkCmdClearColorImage"
    c_vkCmdClearColorImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearColorValue -> Word32 ->
        Ptr VkImageSubresourceRange -> IO ()

foreign import ccall unsafe "vkCmdCopyBuffer"
    c_vkCmdCopyBuffer :: VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr VkBufferCopy -> IO ()

foreign import ccall unsafe "vkCmdDispatch"
    c_vkCmdDispatch :: VkCommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "vkCmdDraw"
    c_vkCmdDraw :: VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "vkCmdDrawIndexed"
    c_vkCmdDrawIndexed :: VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

foreign import ccall unsafe "vkCmdEndRenderPass"
    c_vkCmdEndRenderPass :: VkCommandBuffer -> IO ()

foreign import ccall unsafe "vkCmdExecuteCommands"
    c_vkCmdExecuteCommands :: VkCommandBuffer -> Word32 -> Ptr VkCommandBuffer -> IO ()

foreign import ccall unsafe "vkCmdFillBuffer"
    c_vkCmdFillBuffer :: VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()

foreign import ccall unsafe "vkCmdPipelineBarrier"
    c_vkCmdPipelineBarrier :: VkCommandBuffer -> VkPipelineStageFlags -> VkPipelineStageFlags -> VkDependencyFlags -> Word32 ->
        Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ()

foreign import ccall unsafe "vkCmdPushConstants"
    c_vkCmdPushConstants :: VkCommandBuffer -> VkPipelineLayout -> VkShaderStageFlags -> Word32 -> Word32 -> Ptr Void -> IO ()

foreign import ccall unsafe "vkCmdResolveImage"
    c_vkCmdResolveImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ()

foreign import ccall unsafe "vkCreateCommandPool"
    c_vkCreateCommandPool :: VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool ->
        IO VkResult

foreign import ccall unsafe "vkDestroyCommandPool"
    c_vkDestroyCommandPool :: VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkEndCommandBuffer"
    c_vkEndCommandBuffer :: VkCommandBuffer -> IO VkResult

foreign import ccall unsafe "vkFreeCommandBuffers"
    c_vkFreeCommandBuffers :: VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()


createVkCommandBufferAllocateInfo :: Next -> VkCommandPool -> VkCommandBufferLevel -> CommandBufferCount -> VkCommandBufferAllocateInfo
createVkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo structureTypeCommandBufferAllocateInfo

createVkCommandBufferBeginInfo :: Next -> VkCommandBufferUsageFlagBits -> Maybe VkCommandBufferInheritanceInfo ->
    IO VkCommandBufferBeginInfo
createVkCommandBufferBeginInfo v cBUFB i = do
    p <- fromMaybeIO i
    return $ VkCommandBufferBeginInfo structureTypeCommandBufferBeginInfo v cBUF p
    where
        cBUF = VkCommandBufferUsageFlags $ unVkCommandBufferUsageFlagBits cBUFB

createVkCommandPoolInfo :: Next -> VkCommandPoolCreateFlags -> QueueFamilyIndex -> VkCommandPoolCreateInfo
createVkCommandPoolInfo = VkCommandPoolCreateInfo structureTypeCommandPoolCreateInfo

createVkRenderPassBeginInfo :: Next -> VkRenderPass -> VkFramebuffer -> VkRect2D -> ClearValueCount -> [VkClearValue] -> IO VkRenderPassBeginInfo
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
    Maybe [VkDescriptorSet] -> DynamicOffsetCount -> Maybe [DynamicOffsets] -> IO ()
vkCmdBindDescriptorSets cB pBP pL fS dSC dS dOC dO = do
    pDS <- fromMaybeListIO dSC dS
    pDO <- fromMaybeListIO dOC dO
    c_vkCmdBindDescriptorSets cB pBP pL fS dSC pDS dOC pDO

vkCmdBindIndexBuffer :: VkCommandBuffer -> VkBuffer -> Offset -> VkIndexType -> IO ()
vkCmdBindIndexBuffer = c_vkCmdBindIndexBuffer

vkCmdBindPipeline :: VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()
vkCmdBindPipeline = c_vkCmdBindPipeline

vkCmdBindVertexBuffers :: VkCommandBuffer -> FirstBinding -> BindingCount -> VkBuffer -> VkDeviceSize -> IO ()
vkCmdBindVertexBuffers cB fB bC b dS = alloca $ \pB ->
    alloca $ \pDS -> do
        poke pB b
        poke pDS dS
        c_vkCmdBindVertexBuffers cB fB bC pB pDS

vkCmdBlitImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> RegionCount -> [VkImageBlit] -> VkFilter -> IO ()
vkCmdBlitImage cB sI sIL dI dIL rC iB f = allocaArray i $ \p -> do
    pokeArray p iB
    c_vkCmdBlitImage cB sI sIL dI dIL rC p f
    where
        i = cast rC

vkCmdBuildAccelerationStructureKHR :: VkCommandBuffer -> InfoCount -> [VkAccelerationStructureBuildGeometryInfoKHR] -> [VkAccelerationStructureBuildOffsetInfoKHR] -> IO ()
vkCmdBuildAccelerationStructureKHR cB iC aSBGI aSBOI = allocaArray i $ \pASBGI ->
    allocaArray i $ \pASBOI -> do
        pokeArray pASBGI aSBGI
        pokeArray pASBOI aSBOI
        c_vkCmdBuildAccelerationStructureKHR cB iC pASBGI pASBOI
        where
            i = cast iC

vkCmdClearColorImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> VkClearColorValue -> RangeCount ->
    [VkImageSubresourceRange] -> IO ()
vkCmdClearColorImage cB vkI iL cCV rC iSR = alloca $ \pCCV ->
    allocaArray i $ \pISR -> do
        poke pCCV cCV
        pokeArray pISR iSR
        c_vkCmdClearColorImage cB vkI iL pCCV rC pISR
        where
            i = cast rC

vkCmdCopyBuffer :: VkCommandBuffer -> SrcBuffer -> DstBuffer -> RegionCount -> Regions -> IO ()
vkCmdCopyBuffer cB src dst c ls = allocaArray i $ \p -> do
    pokeArray p ls
    c_vkCmdCopyBuffer cB src dst c p
    where
        i = cast c

vkCmdDispatch :: VkCommandBuffer -> GroupCountX -> GroupCountY -> GroupCountZ -> IO ()
vkCmdDispatch = c_vkCmdDispatch

vkCmdDraw :: VkCommandBuffer -> VertexCount -> InstanceCount -> FirstVertex -> FirstInstance -> IO ()
vkCmdDraw = c_vkCmdDraw

vkCmdDrawIndexed :: VkCommandBuffer -> IndexCount -> InstanceCount -> FirstIndex -> VertexOffset -> FirstInstance -> IO ()
vkCmdDrawIndexed = c_vkCmdDrawIndexed

vkCmdEndRenderPass :: VkCommandBuffer -> IO ()
vkCmdEndRenderPass = c_vkCmdEndRenderPass

vkCmdExecuteCommands :: VkCommandBuffer -> CommandBufferCount -> [VkCommandBuffer] -> IO ()
vkCmdExecuteCommands prime cBC ss = allocaArray i $ \p -> do
    pokeArray p ss
    c_vkCmdExecuteCommands prime cBC p
    where
        i = cast cBC

-- Note: Offset and Size need to be in multiples of 4.
vkCmdFillBuffer :: VkCommandBuffer -> VkBuffer -> Offset -> Size -> Data -> IO ()
vkCmdFillBuffer = c_vkCmdFillBuffer

vkCmdPipelineBarrier :: VkCommandBuffer -> SrcStageMask -> DstStageMask -> VkDependencyFlags -> MemoryBarrierCount ->
    Maybe [VkMemoryBarrier] -> BufferMemoryBarrierCount -> Maybe [VkBufferMemoryBarrier] -> ImageMemoryBarrierCount ->
    Maybe [VkImageMemoryBarrier] -> IO ()
vkCmdPipelineBarrier cB src dst f mBC mB bMBC bMB iMBC iMB = do
    pMB     <- fromMaybeListIO mBC mB
    pBMB    <- fromMaybeListIO bMBC bMB
    pIMB    <- fromMaybeListIO iMBC iMB
    c_vkCmdPipelineBarrier cB src' dst' f mBC pMB bMBC pBMB iMBC pIMB
    where
        src' = VkPipelineStageFlags $ unVkPipelineStageFlagBits src
        dst' = VkPipelineStageFlags $ unVkPipelineStageFlagBits dst

vkCmdPushConstants :: Storable a => VkCommandBuffer -> VkPipelineLayout -> [VkShaderStageFlagBits] -> PushOffset -> PushSize ->
    a -> IO ()
vkCmdPushConstants cB pL sSFB o s v = alloca $ \p -> do
    poke p v
    c_vkCmdPushConstants cB pL sSF o s (castPtr p)
    where
        sSF = VkShaderStageFlags $ vkBits unVkShaderStageFlagBits sSFB

vkCmdResolveImage :: VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> [VkImageResolve] -> IO ()
vkCmdResolveImage cB sI sIL dI dIL iRC iR = allocaArray i $ \p -> do
    pokeArray p iR
    c_vkCmdResolveImage cB sI sIL dI dIL iRC p
    where
        i = cast iRC

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

vkFreeCommandBuffers :: VkDevice -> VkCommandPool -> CommandBufferCount -> [VkCommandBuffer] -> IO()
vkFreeCommandBuffers d cP c b = allocaArray i $ \p -> do
    pokeArray p b
    c_vkFreeCommandBuffers d cP c p
    where
        i = cast c
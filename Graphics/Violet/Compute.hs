{-# LANGUAGE Safe #-}

module Graphics.Violet.Compute where


import Control.Monad (liftM, replicateM)

import Foreign (Storable, peekArray)
import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan

import Graphics.Vulkan.Buffers (vkDestroyBuffer)
import Graphics.Vulkan.Command
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data (VkComponentMapping(..), VkComputePipelineCreateInfo(..), VkDescriptorImageInfo(..), VkDescriptorPoolSize(..), VkImageMemoryBarrier(..))
import Graphics.Vulkan.Descriptor
import Graphics.Vulkan.Devices
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Fences (vkDestroyFence, vkResetFences, vkWaitForFences)
import Graphics.Vulkan.Images
import Graphics.Vulkan.Instance (vkDestroyInstance)
import Graphics.Vulkan.Memory (vkFreeMemory)
import Graphics.Vulkan.Pipelines
import Graphics.Vulkan.Queue (createVkPresentInfoKHR, createVkSubmitInfo, vkQueuePresentKHR, vkQueueSubmit, vkQueueWaitIdle)
import Graphics.Vulkan.Semaphores (vkDestroySemaphore)
import Graphics.Vulkan.Shaders
import Graphics.Vulkan.Surface (vkAcquireNextImageKHR, vkDestroySurfaceKHR, vkDestroySwapchainKHR)
import Graphics.Vulkan.Types

import Loaders.Obj

import Math.Data


createComputeCommandBuffer :: VkDevice -> {-VkBuffer -> VkBuffer ->-} VkCommandPool -> VkImage -> VkPipeline -> VkPipelineLayout -> {-[Word32] ->-} VkDescriptorSet -> ResX -> ResY -> IO VkCommandBuffer
createComputeCommandBuffer vkDev0 {-verBuf indBuf-} cmdPo0 frame pipe layout {-indices-} desSet x y = do
    cmdBuf  <- allocateCommandBuffer vkDev0 cmdPo0 commandBufferLevelPrimary
    cmdBBI  <- createVkCommandBufferBeginInfo nullPtr (VkCommandBufferUsageFlagBits 0) Nothing
    let tracerIMB0 = VkImageMemoryBarrier structureTypeImageMemoryBarrier nullPtr (VkAccessFlags 0) aSW imageLayoutUndefined imageLayoutGeneral queueFamilyIgnored queueFamilyIgnored frame subRan
        tracerIMB1 = VkImageMemoryBarrier structureTypeImageMemoryBarrier nullPtr aSW (VkAccessFlags 0) imageLayoutGeneral imageLayoutPresentSRCKHR queueFamilyIgnored queueFamilyIgnored frame subRan


    _ <- vkBeginCommandBuffer cmdBuf cmdBBI
    -- RayTracer
    vkCmdBindPipeline cmdBuf pipelineBindPointCompute pipe
    -- vkCmdBindVertexBuffers cmdBuf 0 1 verBuf zero
    -- vkCmdBindIndexBuffer cmdBuf indBuf zero indexTypeUInt32
    vkCmdBindDescriptorSets cmdBuf pipelineBindPointCompute layout 0 1 (Just [desSet]) 0 Nothing

    -- vkCmdFillBuffer cmdBuf vkBuff (VkDeviceSize 0) wholeSize 0
    -- vkCmdPushConstants cmdBuf layout [shaderStageComputeBit] 0 4 (0 :: Word)
    vkCmdPipelineBarrier cmdBuf pipelineStageTopOfPipeBit pipelineStageComputeShaderBit (VkDependencyFlags 0) 0 Nothing 0 Nothing 1 (Just [tracerIMB0])

    vkCmdDispatch cmdBuf x' y' 1

    vkCmdPipelineBarrier cmdBuf pipelineStageComputeShaderBit pipelineStageBottomOfPipeBit (VkDependencyFlags 0) 0 Nothing 0 Nothing 1 (Just [tracerIMB1])

    _ <- vkEndCommandBuffer cmdBuf

    return cmdBuf
    where
        aMR     = VkAccessFlags $ unVkAccessFlagBits accessMemoryReadBit
        aSW     = VkAccessFlags $ unVkAccessFlagBits accessShaderWriteBit
        subRan  = createVkImageSubresourceRange [imageAspectColorBit] 0 1 0 1
        x'      = div x 16 + mod x 16
        y'      = div y 16 + mod y 16
        -- indeCo  = fromIntegral $ length indices
        -- zero    = VkDeviceSize 0

createComputeTracerPipeline :: VkDevice -> IO (VkPipelineCache, VkDescriptorSetLayout, VkPipeline, VkPipelineLayout)
createComputeTracerPipeline vkDev0 = do
    vkSMIC  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Violet/Compute/Tracer.c.spv"
    vkSMoC  <- vkCreateShaderModule vkDev0 vkSMIC
    vPSSCI  <- createVkPipelineShaderStageCreateInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageComputeBit vkSMoC "main" Nothing
    vkDSLB  <- createVkDescriptorSetLayoutBinding 0 descriptorTypeStorageImage 1 [shaderStageComputeBit] Nothing
    vDSLCI  <- createVkDescriptorSetLayoutCreateInfo nullPtr (VkDescriptorSetLayoutCreateFlags 0) 1 (Just [vkDSLB])
    vkDSL0  <- vkCreateDescriptorSetLayout vkDev0 vDSLCI
    vkPLCI  <- createVkPipelineLayoutCreateInfo nullPtr (VkPipelineLayoutCreateFlags 0) 1 (Just [vkDSL0]) 0 Nothing
    layout  <- vkCreatePipelineLayout vkDev0 vkPLCI
    vkPCCI  <- createVkPipelineCacheInfo nullPtr (VkPipelineCacheCreateFlags 0) ""
    cache   <- vkCreatePipelineCache vkDev0 vkPCCI
    let vkCPCI = VkComputePipelineCreateInfo structureTypeComputePipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) vPSSCI layout (VkPipeline 0) 0
    vkCoPi <- vkCreateComputePipelines vkDev0 cache 1 [vkCPCI]

    let pipe = head vkCoPi

    vkDestroyShaderModule vkDev0 vkSMoC
    return (cache, vkDSL0, pipe, layout)

draw :: (Num a, Storable a) => VkDevice -> [VkDeviceMemory] -> [VkFence] -> VkSwapchainKHR -> ([VkSemaphore], [VkSemaphore]) -> [VkCommandBuffer] -> [VkPipelineStageFlagBits] -> VkQueue -> Frame -> UBO a -> IO Frame
draw vkDev0 [unifMe] fences swap (semaIm, semaPr) cmdBuf vkPSFB vkQue0 f ubo = do
    _ <- vkWaitForFences vkDev0 1 [fences !! f] vkTrue wait
    nextIm <- vkAcquireNextImageKHR vkDev0 swap wait (semaIm  !! f) $ VkFence nullHandle
    let i = cast nextIm
    _ <- vkWaitForFences vkDev0 1 [fences !! i] vkTrue wait
    updateUniformBuffer vkDev0 unifMe ubo
    vkSuIn <- createVkSubmitInfo nullPtr 1 (Just [semaIm !! f]) (Just vkPSFB) 1 [cmdBuf !! i] 1 $ Just [semaPr !! f]
    _ <- vkResetFences vkDev0 1 [fences !! f]
    _ <- vkQueueSubmit vkQue0 1 [vkSuIn] $ fences !! f
    vkPrIn <- createVkPresentInfoKHR nullPtr 1 [semaPr !! f] 1 [swap] [nextIm]
    _ <- vkQueuePresentKHR vkQue0 vkPrIn
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkWaitForFences vkDev0 1 [fences !! f] vkTrue wait
    return $ mod (f + 1) l
    where
        l   = length cmdBuf
        -- nanoseconds
        wait = 18446744073709551615

initializeCompute :: VkInstance -> VkSurfaceKHR -> Model -> ResX -> ResY -> IO ([VkBuffer], [VkCommandBuffer], VkCommandPool, VkDescriptorPool, [VkDescriptorSetLayout], VkDevice, VkDeviceMemory, [VkFence], [VkImageView], [VkPipeline], [VkPipelineCache], [VkPipelineLayout], [VkPipelineStageFlagBits], VkQueue, ([VkSemaphore], [VkSemaphore]), VkSwapchainKHR)
initializeCompute vkInst vkSurf (Model v n indices) x y = do
    vkDev0 <- createDevice vkInst vkSurf
    (cache, desSet, pipe, layout) <- createComputeTracerPipeline vkDev0
    (fences, swapIs, swapIV, semaph, vkSC) <- initializeSwapChain vkDev0 vkSurf formatB8G8R8A8UNorm [imageUsageStorageBit] x y

    let vkCPIn = createVkCommandPoolInfo nullPtr (VkCommandPoolCreateFlags 0) 0
    vkCPo0  <- vkCreateCommandPool vkDev0 vkCPIn
    vkQue0  <- vkGetDeviceQueue vkDev0 0 0

    -- vkBCI   <- createVkBufferInfo nullPtr (VkBufferCreateFlags 0) (VkDeviceSize 2136746240)
    --     [bufferUsageStorageBufferBit, bufferUsageTransferDSTBit] sharingModeExclusive 3 [0]
    -- vkBuff  <- vkCreateBuffer vkDev0 vkBCI
    -- vkBuMR  <- vkGetBufferMemoryRequirements vkDev0 vkBuff
    -- let vkMAI = createVkMemoryAllocateInfo nullPtr (VkDeviceSize $ 2136746240 + 16) 1
    -- vkDeMe  <- vkAllocateMemory vkDev0 vkMAI
    -- buffMa  <- vkMapMemory vkDev0 vkDeMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    -- buffMB  <- vkBindBufferMemory vkDev0 vkBuff vkDeMe (alignment vkBuMR)

    let vkDPS0 = VkDescriptorPoolSize descriptorTypeStorageImage 5
    vkDPCI <- createVkDescriptorPoolCreateInfo nullPtr (VkDescriptorPoolCreateFlags 0) 5 1 [vkDPS0]
    vkDeP0 <- vkCreateDescriptorPool vkDev0 vkDPCI
    vkDSAI <- createVkDescriptorSetAllocateInfo nullPtr vkDeP0 5 $ replicate 5 desSet
    vkAlDS <- vkAllocateDescriptorSets vkDev0 vkDSAI
    let vkDII0 = map (\x -> VkDescriptorImageInfo (VkSampler 0) x imageLayoutGeneral) swapIV
        vAlDS0 = head vkAlDS
        zippe0 = zip vkDII0 vkAlDS
        zippe1 = zip swapIs vkAlDS
    vWDSI0 <- mapM (\(x, y) -> createVkWriteDescriptorSet nullPtr y 0 0 1 descriptorTypeStorageImage (Just [x]) Nothing Nothing) zippe0
    vkUpdateDescriptorSets vkDev0 5 (Just vWDSI0) 0 Nothing

    (uniBuf, unifMe)    <- createBuffer vkDev0 [bufferUsageUniformBufferBit] (VkDeviceSize $ 4 * 4 * 2) 1

    cmdBuf <- mapM (\(i, j) -> createComputeCommandBuffer vkDev0 vkCPo0 i pipe layout j x y) zippe1

    return ([uniBuf], cmdBuf, vkCPo0, vkDeP0, [desSet], vkDev0, unifMe, fences, swapIV, [pipe], [cache], [layout], vkPSFB, vkQue0, semaph, vkSC)
    where
        vkPSFB  = [pipelineStageComputeShaderBit]
        vkISR0 = createVkImageSubresourceRange [imageAspectColorBit] 0 1 0 1
        vkCMId = VkComponentMapping componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity

--------------------------------------------------------------------------------------------------------------------------------
--
-- Shutdown
--
--------------------------------------------------------------------------------------------------------------------------------
shutdown :: [VkBuffer] -> VkCommandPool -> VkDescriptorPool -> [VkDescriptorSetLayout] -> VkDevice -> [VkDeviceMemory] -> [VkFence] -> [VkImageView] -> VkInstance -> [VkPipeline] -> [VkPipelineCache] -> [VkPipelineLayout] -> VkQueue -> ([VkSemaphore], [VkSemaphore]) -> VkSurfaceKHR -> VkSwapchainKHR -> IO ()
shutdown buffer vkCPo0 vkDeP0 deSLay vkDev0 memory fences imageV vkInst pipe cache pipeLa vkQue0 (semaIm, semaPr) vkSurf vkSC = do
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkDeviceWaitIdle vkDev0
    mapM_ (vkDestroyFence vkDev0) fences
    mapM_ (vkDestroySemaphore vkDev0) semaPr
    mapM_ (vkDestroySemaphore vkDev0) semaIm
    vkDestroyDescriptorPool vkDev0 vkDeP0
    mapM_ (vkDestroyPipelineCache vkDev0) cache
    mapM_ (vkDestroyPipelineLayout vkDev0) pipeLa
    mapM_ (vkDestroyDescriptorSetLayout vkDev0) deSLay
    mapM_ (vkDestroyPipeline vkDev0) pipe
    vkDestroyCommandPool vkDev0 vkCPo0
    mapM_ (vkDestroyBuffer vkDev0) buffer
    mapM_ (vkFreeMemory vkDev0) memory
    mapM_ (vkDestroyImageView vkDev0) imageV
    vkDestroySwapchainKHR vkDev0 vkSC
    vkDestroyDevice vkDev0
    vkDestroySurfaceKHR vkInst vkSurf
    vkDestroyInstance vkInst
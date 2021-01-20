{-# LANGUAGE Safe #-}

module Graphics.Violet.Rasterizer where


import Control.Monad (replicateM)

import Data.Bits ((.|.), shiftR)
import Data.Word (Word8, Word32)

import Foreign (Storable)
import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan

import Graphics.Vulkan.Buffers (vkDestroyBuffer)
import Graphics.Vulkan.Command -- (createVkCommandBufferBeginInfo, createVkCommandPoolInfo, createVkRenderPassBeginInfo, vkBeginCommandBuffer, vkCmdBeginRenderPass, vkCmdBindDescriptorSets, vkCmdBindIndexBuffer, vkCmdBindPipeline, vkCmdBindVertexBuffers, vkCmdDrawIndexed, vkCmdEndRenderPass, vkCreateCommandPool, vkDestroyCommandPool, vkEndCommandBuffer)
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data (VkAttachmentDescription(..), VkAttachmentReference(..), VkClearColorValue(..), VkClearValue(..), VkCommandBufferBeginInfo, VkDescriptorBufferInfo(..), VkExtent2D(..), VkOffset2D(..), VkPipelineColorBlendAttachmentState(..), VkPipelineInputAssemblyStateCreateInfo(..), VkPipelineMultisampleStateCreateInfo(..), VkPipelineRasterizationStateCreateInfo(..), VkPresentInfoKHR(..), VkRect2D(..), VkRenderPassBeginInfo(..), VkSubpassDependency(..), VkVertexInputAttributeDescription(..), VkVertexInputBindingDescription(..), VkViewport(..))
import Graphics.Vulkan.Descriptor (createVkDescriptorSetAllocateInfo, createVkDescriptorSetLayoutBinding, createVkDescriptorSetLayoutCreateInfo, createVkWriteDescriptorSet, vkAllocateDescriptorSets, vkCreateDescriptorSetLayout, vkDestroyDescriptorPool, vkDestroyDescriptorSetLayout, vkUpdateDescriptorSets)
import Graphics.Vulkan.Devices
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Fences (vkDestroyFence, vkResetFences, vkWaitForFences)
import Graphics.Vulkan.Framebuffer
import Graphics.Vulkan.Images
import Graphics.Vulkan.Instance
import Graphics.Vulkan.Memory (vkFreeMemory)
import Graphics.Vulkan.Pipelines
import Graphics.Vulkan.Queue (createVkPresentInfoKHR, createVkSubmitInfo, vkQueuePresentKHR, vkQueueSubmit, vkQueueWaitIdle)
import Graphics.Vulkan.Renderpass
import Graphics.Vulkan.Semaphores (vkDestroySemaphore)
import Graphics.Vulkan.Shaders
import Graphics.Vulkan.Surface
import Graphics.Vulkan.Types

import Loaders.Obj

import Math.Data


createFramebuffer :: VkDevice -> VkRenderPass -> VkImageView -> IO VkFramebuffer
createFramebuffer vkDev0 rendPa imageV = do
    fraCIn  <- createVkFramebufferCreateInfo nullPtr (VkFramebufferCreateFlags 0) rendPa 1 [imageV] 1600 900 1
    vkCreateFramebuffer vkDev0 fraCIn

createGraphicsCommandBuffer :: VkDevice -> VkBuffer -> VkBuffer -> VkCommandPool -> VkPipeline -> VkPipelineLayout -> VkRenderPass -> [Word32] -> (VkDescriptorSet, VkFramebuffer) -> IO VkCommandBuffer
createGraphicsCommandBuffer vkDev0 verBuf indBuf cmdPo0 pipe layout rendPa indices (desSet, frameB) = do
    cmdBuf  <- allocateCommandBuffer vkDev0 cmdPo0
    cmdBBI  <- createVkCommandBufferBeginInfo nullPtr (VkCommandBufferUsageFlagBits 0) Nothing
    let vkCCVa = VkClearColorValue 0 0 0 0
        vkClVa = VkClearValueC vkCCVa
    renPBI  <- createVkRenderPassBeginInfo nullPtr rendPa frameB rendAr 1 [vkClVa]

    _ <- vkBeginCommandBuffer cmdBuf cmdBBI
    vkCmdBeginRenderPass cmdBuf renPBI subpassContentsInline
    vkCmdBindPipeline cmdBuf pipelineBindPointGraphics pipe
    vkCmdBindVertexBuffers cmdBuf 0 1 verBuf zero
    vkCmdBindIndexBuffer cmdBuf indBuf zero indexTypeUInt32
    vkCmdBindDescriptorSets cmdBuf pipelineBindPointGraphics layout 0 1 (Just [desSet]) 0 Nothing
    vkCmdDrawIndexed cmdBuf indeCo 1 0 0 0
    vkCmdEndRenderPass cmdBuf
    _ <- vkEndCommandBuffer cmdBuf
    return cmdBuf
    where
        indeCo  = fromIntegral $ length indices
        rendAr  = VkRect2D (VkOffset2D 0 0) (VkExtent2D 1600 900)
        zero    = VkDeviceSize 0

createGraphicsPipeline :: VkDevice -> VkRenderPass -> IO (VkPipelineLayout, [VkPipeline], VkDescriptorSetLayout)
createGraphicsPipeline vkDev0 vkRePa = do
    vkSMIV  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Violet/Rasterizer/Simple.v.spv"
    vkSMIF  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Violet/Rasterizer/Simple.f.spv"
    vkSMoV  <- vkCreateShaderModule vkDev0 vkSMIV
    vkSMoF  <- vkCreateShaderModule vkDev0 vkSMIF
    vPSSIV  <- createVkPipelineShaderStageCreateInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageVertexBit vkSMoV "main" Nothing
    vPSSIF  <- createVkPipelineShaderStageCreateInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageFragmentBit vkSMoF "main" Nothing

    vkDSLB  <- createVkDescriptorSetLayoutBinding 0 descriptorTypeUniformBuffer 1 [shaderStageVertexBit] Nothing
    vDSLCI  <- createVkDescriptorSetLayoutCreateInfo nullPtr (VkDescriptorSetLayoutCreateFlags 0) 1 (Just [vkDSLB])
    vkDSL0  <- vkCreateDescriptorSetLayout vkDev0 vDSLCI

    let vkVIBD = VkVertexInputBindingDescription 0 (3*4) vertexInputRateVertex
        vVIADV = VkVertexInputAttributeDescription 0 0 vertexXYZFloat 0
        vVIADC = VkVertexInputAttributeDescription 1 0 vertexXYZFloat 0
    vPVICI  <- createVkPipelineVertexInputStateCreateInfo nullPtr (VkPipelineVertexInputStateCreateFlags 0) 1 (Just [vkVIBD]) 2 (Just [vVIADV, vVIADC])
    let vPISCI = VkPipelineInputAssemblyStateCreateInfo structureTypePipelineInputAssembyStateCreateInfo nullPtr (VkPipelineInputAssemblyStateCreateFlags 0) primitiveTopologyTriangleList vkFalse
        vkView = VkViewport 0 0 1600 900 0 1
        scisso = VkRect2D (VkOffset2D 0 0) (VkExtent2D 1600 900)
    vPVSCI  <- createVkPipelineViewportStateCreateInfo nullPtr (VkPipelineViewportStateCreateFlags 0) 1 [vkView] 1 [scisso]
    let vPRSCI = VkPipelineRasterizationStateCreateInfo structureTypePipelineRasterizationStateCreateInfo nullPtr (VkPipelineRasterizationStateCreateFlags 0) vkFalse vkFalse polygonModeFill (VkCullModeFlags $ unVkCullModeFlagBits cullModeBackBit) frontFaceClockwise vkFalse 0 0 0 1
        vPMSCI = VkPipelineMultisampleStateCreateInfo structureTypePipelineMultisampleStateCreateInfo nullPtr (VkPipelineMultisampleStateCreateFlags 0) sampleCount1Bit vkFalse 1 nullPtr vkFalse vkFalse
        vPCBAS = VkPipelineColorBlendAttachmentState vkFalse blendFactorOne blendFactorZero blendOpAdd blendFactorOne blendFactorZero blendOpAdd colorComponentRGBA
    vPCBCI  <- createVkPipelineColorBlendStateCreateInfo nullPtr (VkPipelineColorBlendStateCreateFlags 0) vkFalse logicOpCopy 1 [vPCBAS] [0,0,0,0]
    vPDSCI  <- createVkPipelineDynamicStateCreateInfo nullPtr (VkPipelineDynamicStateCreateFlags 0) 2 [dynamicStateViewport, dynamicStateLineWidth]
    vkPLCI  <- createVkPipelineLayoutCreateInfo nullPtr (VkPipelineLayoutCreateFlags 0)  1 (Just [vkDSL0]) 0 Nothing
    vkPiLa  <- vkCreatePipelineLayout vkDev0 vkPLCI

    vkGPCI  <- createVkGraphicsPipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) 2 [vPSSIV, vPSSIF] vPVICI vPISCI Nothing vPVSCI vPRSCI vPMSCI Nothing vPCBCI Nothing vkPiLa vkRePa 0 (VkPipeline nullHandle) (-1)
    graphP <- vkCreateGraphicsPipelines vkDev0 (VkPipelineCache nullHandle) 1 [vkGPCI]

    vkDestroyShaderModule vkDev0 vkSMoV
    vkDestroyShaderModule vkDev0 vkSMoF
    return (vkPiLa, graphP, vkDSL0)
    where
        vertexXYZFloat      = formatR32G32B32SFloat
        colorComponentRGBA  = VkColorComponentFlags $ unVkColorComponentFlagBits colorComponentRBit .|. unVkColorComponentFlagBits colorComponentGBit .|. unVkColorComponentFlagBits colorComponentBBit .|. unVkColorComponentFlagBits colorComponentABit

createRenderpass :: VkDevice -> IO VkRenderPass
createRenderpass vkDev0 = do
    vkSuD0 <- createVkSubpassDescription (VkSubpassDescriptionFlagBits 0) pipelineBindPointGraphics 0 Nothing 1 (Just [vkARCo]) Nothing Nothing 0 Nothing
    vkRPCI <- createVkRenderPassCreateInfo nullPtr (VkRenderPassCreateFlags 0) 1 (Just [vkADCo]) 1 (Just [vkSuD0]) 1 (Just [vkSuPa])
    vkCreateRenderPass vkDev0 vkRPCI
    where
        vkADCo = VkAttachmentDescription (VkAttachmentDescriptionFlags 0) formatB8G8R8A8SRGB sampleCount1Bit attachmentLoadOpClear attachmentStoreOpStore attachmentLoadOpDontCare attachmentStoreOpDontCare imageLayoutUndefined imageLayoutPresentSRCKHR
        vkARCo = VkAttachmentReference 0 imageLayoutColorAttachmentOptimal
        vkSuPa = VkSubpassDependency subpassExternal 0 subStageMask subStageMask (VkAccessFlags 0) (VkAccessFlags $ unVkAccessFlagBits accessColorAttachmentWriteBit) (VkDependencyFlags 0)
        subStageMask = VkPipelineStageFlags $ unVkPipelineStageFlagBits pipelineStageColorAttachmentOutputBit

draw :: (Num a, Storable a) => VkDevice -> [VkDeviceMemory] -> [VkFence] -> VkSwapchainKHR -> ([VkSemaphore], [VkSemaphore]) -> [VkCommandBuffer] -> [VkPipelineStageFlagBits] -> VkQueue -> Frame -> UBO a -> IO Frame
draw vkDev0 unifMe fences swap (semaIm, semaPr) vkCoBu vkPSFB vkQue0 f ubo = do
    _ <- vkWaitForFences vkDev0 1 [fences !! f] vkTrue wait
    nextIm <- vkAcquireNextImageKHR vkDev0 swap wait (semaIm  !! f) $ VkFence nullHandle
    _ <- vkWaitForFences vkDev0 1 [fences !! cast nextIm] vkTrue wait
    updateUniformBuffer vkDev0 (unifMe !! f) ubo
    vkSuIn <- createVkSubmitInfo nullPtr 1 (Just [semaIm !! f]) (Just vkPSFB) 1 [vkCoBu !! f] 1 $ Just [semaPr !! f]
    _ <- vkResetFences vkDev0 1 [fences !! f]
    _ <- vkQueueSubmit vkQue0 1 [vkSuIn] $ fences !! f
    vkPrIn <- createVkPresentInfoKHR nullPtr 1 [semaPr !! f] 1 [swap] [nextIm]
    _ <- vkQueuePresentKHR vkQue0 vkPrIn
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkWaitForFences vkDev0 1 [fences !! f] vkTrue wait
    return $ mod (f + 1) l
    where
        l   = length vkCoBu
        wait = 18446744073709551615

initializeRasterizer :: VkInstance -> VkSurfaceKHR -> Model -> IO ([VkBuffer], [VkCommandBuffer], VkCommandPool, VkDescriptorPool, VkDescriptorSetLayout, VkDevice, [VkDeviceMemory], [VkDeviceMemory], [VkFence], [VkFramebuffer], [VkImageView], VkPipeline, VkPipelineLayout, [VkPipelineStageFlagBits], VkQueue, VkRenderPass, ([VkSemaphore], [VkSemaphore]), VkSwapchainKHR)
initializeRasterizer vkInst vkSurf (Model v n indices) = do
    vkDev0 <- createDevice vkInst vkSurf
    vkRePa <- createRenderpass vkDev0
    (vkPLGr, [graphP], uboDSL) <- createGraphicsPipeline vkDev0 vkRePa
    (fences, swapIs, swapIV, semaph, vkSC) <- initializeSwapChain vkDev0 vkSurf [imageUsageColorAttachmentBit]


    vFrame  <- mapM (createFramebuffer vkDev0 vkRePa) swapIV

    let vkCPIn  = createVkCommandPoolInfo nullPtr (VkCommandPoolCreateFlags 0) 0
        sCC     = length swapIs
        sCC'    = cast sCC
    vkCPo0  <- vkCreateCommandPool vkDev0 vkCPIn
    vkQue0  <- vkGetDeviceQueue vkDev0 0 0

    (stageV, stagVM) <- createStagingVertexBuffer vkDev0 vertLi
    (vertex, vertMe) <- createBuffer vkDev0 [bufferUsageTransferDSTBit, bufferUsageVertexBufferBit] sV 0
    copyBuffer vkDev0 stageV vertex vkCPo0 sV vkQue0

    (stageC, stagCM) <- createStagingVertexBuffer vkDev0 $ take (cast vertCo) $ cycle colors
    (color, coloMe)  <- createBuffer vkDev0 [bufferUsageTransferDSTBit, bufferUsageVertexBufferBit] sV 0
    copyBuffer vkDev0 stageC color vkCPo0 sV vkQue0

    (stageI, stagIM) <- createStagingIndexBuffer vkDev0 indices
    (index, indeMe) <- createBuffer vkDev0 [bufferUsageTransferDSTBit, bufferUsageIndexBufferBit] sI 0
    copyBuffer vkDev0 stageI index vkCPo0 sI vkQue0

    unifor  <- replicateM sCC $ createBuffer vkDev0 [bufferUsageUniformBufferBit] (VkDeviceSize $ 4 * 4 * 2) 1
    let (uniBuf, unifMe) = unzip unifor
    vkDeP0  <- createUniformDescriptorPool vkDev0 sCC'
    vkDSAI  <- createVkDescriptorSetAllocateInfo nullPtr vkDeP0 sCC' $ replicate sCC uboDSL
    vkAlDS  <- vkAllocateDescriptorSets vkDev0 vkDSAI
    let uniDBI = map (\x -> VkDescriptorBufferInfo x (VkDeviceSize 0) wholeSize) uniBuf
    vkWDSU  <- mapM (\x -> createVkWriteDescriptorSet nullPtr (vkAlDS !! x) 0 0 1 descriptorTypeUniformBuffer Nothing (Just $ uniDBI !! x) Nothing) [0 .. (sCC - 1)]
    mapM_ (\x -> vkUpdateDescriptorSets vkDev0 1 (Just [x]) 0 Nothing) vkWDSU

    let zipped = zip vkAlDS vFrame
    vkCoBu  <- mapM (createGraphicsCommandBuffer vkDev0 vertex index vkCPo0 graphP vkPLGr vkRePa indices) zipped


    return ([color, index, stageC, stageI, stageV, vertex] ++ uniBuf, vkCoBu, vkCPo0, vkDeP0, uboDSL, vkDev0, [coloMe, indeMe, stagCM, stagIM, stagVM, vertMe], unifMe, fences, vFrame, swapIV, graphP, vkPLGr, vkPSFB, vkQue0, vkRePa, semaph, vkSC)
    where
        sI      = VkDeviceSize $ 4 * cast (length indices)
        sV      = VkDeviceSize $ 4 * cast vertCo
        vertCo  = fromIntegral (length vertLi) :: Word32
        vertLi  = concatMap vertex3ToList v
        vkPSFB  = [pipelineStageColorAttachmentOutputBit]
        colors  = [0.1, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0]

--------------------------------------------------------------------------------------------------------------------------------
--
-- Shutdown
--
--------------------------------------------------------------------------------------------------------------------------------
shutdown :: [VkBuffer] -> VkCommandPool -> VkDescriptorPool -> VkDescriptorSetLayout -> VkDevice -> [VkDeviceMemory] -> [VkFence] -> [VkFramebuffer] -> [VkImageView] -> VkInstance -> VkPipeline -> VkPipelineLayout -> VkQueue -> VkRenderPass -> ([VkSemaphore], [VkSemaphore]) -> VkSurfaceKHR -> VkSwapchainKHR -> IO ()
shutdown buffer vkCPo0 vkDeP0 deSLay vkDev0 memory fences vFrame imageV vkInst pipe pipeLa vkQue0 vkRePa (semaIm, semaPr) vkSurf vkSC = do
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkDeviceWaitIdle vkDev0
    mapM_ (vkDestroyFence vkDev0) fences
    mapM_ (vkDestroySemaphore vkDev0) semaPr
    mapM_ (vkDestroySemaphore vkDev0) semaIm
    mapM_ (vkDestroyFramebuffer vkDev0) vFrame
    vkDestroyRenderPass vkDev0 vkRePa
    vkDestroyDescriptorPool vkDev0 vkDeP0
    vkDestroyPipelineLayout vkDev0 pipeLa
    vkDestroyDescriptorSetLayout vkDev0 deSLay
    vkDestroyPipeline vkDev0 pipe
    vkDestroyCommandPool vkDev0 vkCPo0
    mapM_ (vkDestroyBuffer vkDev0) buffer
    mapM_ (vkFreeMemory vkDev0) memory
    mapM_ (vkDestroyImageView vkDev0) imageV
    vkDestroySwapchainKHR vkDev0 vkSC
    vkDestroyDevice vkDev0
    vkDestroySurfaceKHR vkInst vkSurf
    vkDestroyInstance vkInst
{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Control.Monad (replicateM)

import Data.Bits ((.|.), shiftR)
import Data.ByteString (ByteString, hPut, pack)
import Data.Word (Word8, Word32)

import Foreign (Storable, allocaArray, pokeArray)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (castPtr, nullPtr)

import GHC.IO.Handle.Text (memcpy)

import Graphics.Utilities

import Graphics.Vulkan.Buffers
import Graphics.Vulkan.Command
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data (VkAttachmentDescription(..), VkAttachmentReference(..), VkBufferCopy(VkBufferCopy), VkClearValue(..), VkCommandBufferBeginInfo(..), VkComponentMapping(..), VkComputePipelineCreateInfo(..), VkDescriptorBufferInfo(..), VkDescriptorPoolSize(..), VkExtent2D(..), VkExtent3D(..), VkFenceCreateInfo(..), VkImageSubresourceRange(..), VkImageViewCreateInfo(..), VkMemoryRequirements(..), VkOffset2D(..), VkPipelineColorBlendAttachmentState(..), VkPipelineInputAssemblyStateCreateInfo(..), VkPipelineMultisampleStateCreateInfo(..), VkPipelineRasterizationStateCreateInfo(..), VkPresentInfoKHR(..), VkRect2D(..), VkRenderPassBeginInfo(..), VkSubpassDependency(..), VkVertexInputAttributeDescription(..), VkVertexInputBindingDescription(..), VkViewport(..))
import Graphics.Vulkan.Descriptor
import Graphics.Vulkan.Devices
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Fences
import Graphics.Vulkan.Framebuffer
import Graphics.Vulkan.Images
import Graphics.Vulkan.Instance
import Graphics.Vulkan.Memory
import Graphics.Vulkan.Pipelines
import Graphics.Vulkan.Queue
import Graphics.Vulkan.Renderpass
import Graphics.Vulkan.Semaphores
import Graphics.Vulkan.Shaders
import Graphics.Vulkan.Surface
import Graphics.Vulkan.Types

import Loaders.Obj

import Math.Data


-- Type aliases.
type Frame = Int

data Pipeline = Pipeline{
    cache :: VkPipelineCache,
    descriptor :: VkDescriptorSetLayout,
    handle :: VkPipeline,
    layout :: VkPipelineLayout
}

frameTime :: Float
frameTime = 1/200

vertex2ToList :: Num a => Vertex2 a -> [a]
vertex2ToList (Vertex2 x y)             = [x, y]
vertex3ToList :: Num a => Vertex3 a -> [a]
vertex3ToList (Vertex3 x y z)           = [x, y, z]
vertex4ToList :: Num a => Vertex4 a -> [a]
vertex4ToList (Vertex4 x y z w)         = [x, y, z, w]

uboToList :: Num a => UBO a -> [a]
uboToList (UBO (Quaternion x1 y1 z1 w1) (Quaternion x2 y2 z2 w2))= [x1, y1, z1, w1, x2, y2, z2, w2]

allocateCommandBuffer :: VkDevice -> VkCommandPool -> IO VkCommandBuffer
allocateCommandBuffer vkDev0 cmdPo0 = do
    vkCoBu  <- vkAllocateCommandBuffers vkDev0 vkCBAI
    return $ head vkCoBu
    where
        vkCBAI = createVkCommandBufferAllocateInfo nullPtr cmdPo0 commandBufferLevelPrimary 1

copyBuffer :: VkDevice -> VkBuffer -> VkBuffer -> VkCommandPool -> VkDeviceSize -> VkQueue -> IO ()
copyBuffer vkDev0 src dst cmdPo0 s q = do
    cmdBuf  <- allocateCommandBuffer vkDev0 cmdPo0
    cmdBBI  <- createVkCommandBufferBeginInfo nullPtr commandBufferUsageOneSubmitBit Nothing
    _ <- vkBeginCommandBuffer cmdBuf cmdBBI
    vkCmdCopyBuffer cmdBuf src dst 1 [buffCo]
    _ <- vkEndCommandBuffer cmdBuf
    vkSuIn <- createVkSubmitInfo nullPtr 0 Nothing Nothing 1 [cmdBuf] 0 Nothing
    _ <- vkQueueSubmit q 1 [vkSuIn] $ VkFence nullHandle
    _ <- vkQueueWaitIdle q
    vkFreeCommandBuffers vkDev0 cmdPo0 1 [cmdBuf]
    where
        zero    = VkDeviceSize 0
        buffCo  = VkBufferCopy zero zero s

createBuffer :: VkDevice -> [VkBufferUsageFlagBits] -> VkDeviceSize -> Word32 -> IO (VkBuffer, VkDeviceMemory)
createBuffer vkDev0 f dS i = do
    vkBCI   <- createVkBufferInfo  nullPtr (VkBufferCreateFlags 0) dS f sharingModeExclusive 0 [0]
    buffer  <- vkCreateBuffer vkDev0 vkBCI
    vkBuMR  <- vkGetBufferMemoryRequirements vkDev0 buffer
    let vkMAI = createVkMemoryAllocateInfo nullPtr (size vkBuMR) i
    bufMem  <- vkAllocateMemory vkDev0 vkMAI
    _  <- vkBindBufferMemory vkDev0 buffer bufMem (VkDeviceSize 0)
    return (buffer, bufMem)

createComputePipeline :: VkDevice -> IO Pipeline
createComputePipeline vkDev0 = do
    vkSMIC  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.c.spv"
    vkSMoC  <- vkCreateShaderModule vkDev0 vkSMIC
    vkPSIC  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageComputeBit vkSMoC "main" Nothing
    vkDSLB  <- createVkDescriptorSetLayoutBinding 0 descriptorTypeStorageBuffer 1 [shaderStageComputeBit] Nothing
    vDSLCI  <- createVkDescriptorSetLayoutCreateInfo nullPtr (VkDescriptorSetLayoutCreateFlags 0) 1 (Just [vkDSLB])
    vkDSL0  <- vkCreateDescriptorSetLayout vkDev0 vDSLCI
    vkPLCI  <- createVkPipelineLayoutCreateInfo nullPtr (VkPipelineLayoutCreateFlags 0) 1 (Just [vkDSL0]) 0 Nothing
    vkPiLa  <- vkCreatePipelineLayout vkDev0 vkPLCI
    vkPCCI  <- createVkPipelineCacheInfo nullPtr (VkPipelineCacheCreateFlags 0) ""
    vkPiCa  <- vkCreatePipelineCache vkDev0 vkPCCI
    let vkCPCI = VkComputePipelineCreateInfo structureTypeComputePipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) vkPSIC vkPiLa (VkPipeline 0) 0
    vkCoPi <- vkCreateComputePipelines vkDev0 vkPiCa 1 [vkCPCI]

    vkDestroyShaderModule vkDev0 vkSMoC
    return $ Pipeline vkPiCa vkDSL0 (head vkCoPi) vkPiLa

createDevice :: VkInstance -> VkSurfaceKHR -> IO VkDevice
createDevice vkInst vkSurf = do
    vkPDs   <- vkEnumeratePhysicalDevices vkInst
    let vkPD0  = head vkPDs
    vkPDF2  <- vkGetPhysicalDeviceFeatures2 vkPD0
    _       <- vkGetPhysicalDeviceSurfaceSupport vkPD0 0 vkSurf
    vkDQCI  <- createVkDeviceQueueCreateInfo nullPtr (VkDeviceQueueCreateFlags 0) 0 1 [1.0]
    vkDCI   <- createVkDeviceCreateInfo (castPtr vkPDF2) (VkDeviceCreateFlags 0) 1 vkDQCI 1 ["VK_KHR_swapchain"] Nothing
    vkCreateDevice vkPD0 vkDCI

createFramebuffer :: VkDevice -> VkRenderPass -> VkImageView -> IO VkFramebuffer
createFramebuffer vkDev0 rendPa imageV = do
    fraCIn  <- createVkFramebufferCreateInfo nullPtr (VkFramebufferCreateFlags 0) rendPa 1 [imageV] 1600 900 1
    vkCreateFramebuffer vkDev0 fraCIn

createGraphicsCommandBuffer :: VkDevice -> VkBuffer -> VkBuffer -> VkCommandPool -> VkPipeline -> VkPipelineLayout -> VkRenderPass -> [Word32] -> (VkDescriptorSet, VkFramebuffer) -> IO VkCommandBuffer
createGraphicsCommandBuffer vkDev0 verBuf indBuf cmdPo0 graphP layout rendPa indices (desSet, frameB) = do
    cmdBuf  <- allocateCommandBuffer vkDev0 cmdPo0
    cmdBBI  <- createVkCommandBufferBeginInfo nullPtr (VkCommandBufferUsageFlagBits 0) Nothing
    vkCCVa  <- createVkClearColorValue [0,0,0,0]
    let vkClVa = VkClearValueC vkCCVa
    renPBI  <- createVkRenderPassBeginInfo nullPtr rendPa frameB rendAr 1 [vkClVa]

    _ <- vkBeginCommandBuffer cmdBuf cmdBBI
    vkCmdBeginRenderPass cmdBuf renPBI subpassContentsInline
    vkCmdBindPipeline cmdBuf pipelineBindPointGraphics graphP
    vkCmdBindVertexBuffers cmdBuf 0 1 verBuf zero
    vkCmdBindIndexBuffer cmdBuf indBuf zero indexTypeUInt32
    vkCmdBindDescriptorSets cmdBuf pipelineBindPointGraphics layout 0 1 [desSet] 0 Nothing
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
    vkSMIV  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.v.spv"
    vkSMIF  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.f.spv"
    vkSMoV  <- vkCreateShaderModule vkDev0 vkSMIV
    vkSMoF  <- vkCreateShaderModule vkDev0 vkSMIF
    vkPSIV  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageVertexBit vkSMoV "main" Nothing
    vkPSIF  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageFragmentBit vkSMoF "main" Nothing

    vkDSLB  <- createVkDescriptorSetLayoutBinding 0 descriptorTypeUniformBuffer 1 [shaderStageVertexBit] Nothing
    vDSLCI  <- createVkDescriptorSetLayoutCreateInfo nullPtr (VkDescriptorSetLayoutCreateFlags 0) 1 (Just [vkDSLB])
    vkDSL0  <- vkCreateDescriptorSetLayout vkDev0 vDSLCI

    let vkVIBD = VkVertexInputBindingDescription 0 (3*4) vertexInputRateVertex
        vVIADV = VkVertexInputAttributeDescription 0 0 formatR32G32B32SFloat 0
        vVIADC = VkVertexInputAttributeDescription 1 0 formatR32G32B32SFloat 0
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

    vkGPCI  <- createVkGraphicsPipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) 2 [vkPSIV, vkPSIF] vPVICI vPISCI Nothing vPVSCI vPRSCI vPMSCI Nothing vPCBCI Nothing vkPiLa vkRePa 0 (VkPipeline nullHandle) (-1)
    graphP <- vkCreateGraphicsPipelines vkDev0 (VkPipelineCache nullHandle) 1 [vkGPCI]

    vkDestroyShaderModule vkDev0 vkSMoV
    vkDestroyShaderModule vkDev0 vkSMoF
    return (vkPiLa, graphP, vkDSL0)
    where
        colorComponentRGBA = VkColorComponentFlags $ unVkColorComponentFlagBits colorComponentRBit .|. unVkColorComponentFlagBits colorComponentGBit .|. unVkColorComponentFlagBits colorComponentBBit .|. unVkColorComponentFlagBits colorComponentABit

createInstance :: IO VkInstance
createInstance = do
    let api = makeAPI 1 2 141
    vkApIn  <- createVkApplicationInfo nullPtr "Nightmare" 0 "Nightmare" 0 api
    vkInCI  <- createVkInstanceCreateInfo nullPtr 0 (Just vkApIn) 1 (Just ["VK_LAYER_KHRONOS_validation"]) 3
        (Just ["VK_EXT_debug_report", "VK_KHR_surface", "VK_KHR_xlib_surface"])
    vkCreateInstance vkInCI

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

createStagingIndexBuffer :: VkDevice -> [Word32] -> IO (VkBuffer, VkDeviceMemory)
createStagingIndexBuffer vkDev0 indices = allocaArray l $ \p -> do
    pokeArray p indices
    (stage, stagMe) <- createBuffer vkDev0 [bufferUsageTransferSRCBit] s 2
    buffMa <- vkMapMemory vkDev0 stagMe (VkDeviceSize 0) s (VkMemoryMapFlags 0)
    _ <- memcpy (castPtr buffMa) p $ CSize $ 4 * w
    vkUnmapMemory vkDev0 stagMe

    return (stage, stagMe)
    where
        s = VkDeviceSize $ 4 * w
        l = length indices
        w = cast l

createStagingVertexBuffer :: VkDevice -> [Float] -> IO (VkBuffer, VkDeviceMemory)
createStagingVertexBuffer vkDev0 vertLi = allocaArray l $ \p -> do
    pokeArray p vertLi
    (stage, stagMe) <- createBuffer vkDev0 [bufferUsageTransferSRCBit] s 2
    buffMa <- vkMapMemory vkDev0 stagMe (VkDeviceSize 0) s (VkMemoryMapFlags 0)
    _ <- memcpy (castPtr buffMa) p $ CSize $ 4 * w
    vkUnmapMemory vkDev0 stagMe

    return (stage, stagMe)
    where
        s = VkDeviceSize $ 4 * w
        l = length vertLi
        w = cast l

createSwapchain :: VkDevice -> VkSurfaceKHR -> IO (VkSwapchainKHR, [VkImageView])
createSwapchain vkDev0 vkSurf = do
    vkSCCI  <- createVkSwapchainCreateInfo nullPtr (VkSwapchainCreateFlagsKHR 0) vkSurf 3 formatB8G8R8A8SRGB colorSpaceSRGBNonlinearKHR
                    (VkExtent2D 1600 900) 1 imageUsageColorAttachmentBit sharingModeExclusive 1 [0] surfaceTransformIdentityBitKHR
                    compositeAlphaOpaqueBitKHR presentModeFIFOKHR vkTrue (VkSwapchainKHR nullHandle)
    vkSC    <- vkCreateSwapchainKHR vkDev0 vkSCCI
    vkSCIs  <- vkGetSwapchainImagesKHR vkDev0 vkSC
    let vkIVCI = map (\x -> VkImageViewCreateInfo structureTypeImageViewCreateInfo nullPtr (VkImageViewCreateFlags 0) x
                    imageViewType2D formatB8G8R8A8SRGB vkCMId vkISR0) vkSCIs
    imageV   <- mapM (vkCreateImageView vkDev0) vkIVCI

    return (vkSC, imageV)
    where
        vkISR0 = createVkImageSubresourceRange [imageAspectColorBit] 0 1 0 1
        vkCMId = VkComponentMapping componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity

createSwapchainFence :: VkDevice -> IO VkFence
createSwapchainFence vkDev0 = vkCreateFence vkDev0 vkFCI
    where
        vkFCI = VkFenceCreateInfo structureTypeFenceCreateInfo nullPtr $ VkFenceCreateFlags $ unVkFenceCreateFlagBits fenceCreateSignaledBit

createSwapchainSemaphores :: VkDevice -> IO ([VkSemaphore], [VkSemaphore])
createSwapchainSemaphores vkDev0 = do
    vkSTCI <- createVkSemaphoreTypeCreateInfo nullPtr vkSemaphoreTypeBinary 0
    vkSem0 <- replicateM 3 $ vkCreateSemaphore vkDev0 vkSTCI
    vkSem1 <- replicateM 3 $ vkCreateSemaphore vkDev0 vkSTCI
    return (vkSem0, vkSem1)

updateUniformBuffer :: (Num a, Storable a) => VkDevice -> VkDeviceMemory -> UBO a -> IO ()
updateUniformBuffer vkDev0 uniMe ubo = allocaArray l $ \p -> do
    pokeArray p u
    buffMa <- vkMapMemory vkDev0 uniMe (VkDeviceSize 0) s (VkMemoryMapFlags 0)
    _ <- memcpy (castPtr buffMa) p $ CSize $ 4 * w
    vkUnmapMemory vkDev0 uniMe
    where
        u = uboToList ubo
        s = VkDeviceSize $ 4 * w
        l = length u
        w = cast l

createUniformDescriptorPool :: VkDevice -> IO VkDescriptorPool
createUniformDescriptorPool vkDev0 = do
    vkDPCI <- createVkDescriptorPoolCreateInfo nullPtr (VkDescriptorPoolCreateFlags 0) 3 1 [vkDPS0]
    vkCreateDescriptorPool vkDev0 vkDPCI
    where
        vkDPS0 = VkDescriptorPoolSize descriptorTypeUniformBuffer 3

draw :: (Num a, Storable a) => VkDevice -> [VkDeviceMemory] -> [VkFence] -> VkSwapchainKHR -> ([VkSemaphore], [VkSemaphore]) -> [VkCommandBuffer] -> VkQueue -> Frame -> UBO a -> IO Frame
draw vkDev0 unifMe fences vkSC (semaIm, semaPr) vkCoBu vkQue0 f ubo = do
    _ <- vkWaitForFences vkDev0 1 [fences !! f] vkTrue 18446744073709551615
    nextIm <- vkAcquireNextImageKHR vkDev0 vkSC 18446744073709551615 (semaIm  !! f) $ VkFence nullHandle
    _ <- vkWaitForFences vkDev0 1 [fences !! cast nextIm] vkTrue 18446744073709551615
    updateUniformBuffer vkDev0 (unifMe !! f) ubo
    vkSuIn <- createVkSubmitInfo nullPtr 1 (Just [semaIm !! f]) (Just [pipelineStageColorAttachmentOutputBit]) 1 [vkCoBu !! f] 1 $ Just [semaPr !! f]
    _ <- vkResetFences vkDev0 1 [fences !! f]
    _ <- vkQueueSubmit vkQue0 1 [vkSuIn] $ fences !! f
    vkPrIn <- createVkPresentInfoKHR nullPtr 1 [semaPr !! f] 1 [vkSC] [nextIm]
    _ <- vkQueuePresentKHR vkQue0 vkPrIn
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkWaitForFences vkDev0 1 [fences !! f] vkTrue 18446744073709551615
    return $ mod (f + 1) 3

initialize :: VkInstance -> VkSurfaceKHR -> Model -> IO ([VkBuffer], [VkCommandBuffer], VkCommandPool, VkDescriptorPool, [VkDescriptorSetLayout], VkDevice, [VkDeviceMemory], [VkDeviceMemory], [VkFence], [VkFramebuffer], VkImage, [VkImageView], [VkPipeline], VkPipelineCache, [VkPipelineLayout], VkQueue, VkRenderPass, ([VkSemaphore], [VkSemaphore]), VkSwapchainKHR)
initialize vkInst vkSurf (Model v n indices) = do
    vkDev0 <- createDevice vkInst vkSurf
    vkRePa <- createRenderpass vkDev0
    (vkPLGr, [graphP], uboDSL) <- createGraphicsPipeline vkDev0 vkRePa
    comput <- createComputePipeline vkDev0
    swapCh <- createSwapchain vkDev0 vkSurf
    semaph <- createSwapchainSemaphores vkDev0
    fences <- replicateM sCS $ createSwapchainFence vkDev0
    let vkPiCa = cache comput
        compPi = handle comput
        vkPLCo = Graphics.Vulkan.layout comput
        vkDSL0 = descriptor comput
        vkSC   = fst swapCh
        swapIV = snd swapCh


    vFrame  <- mapM (createFramebuffer vkDev0 vkRePa) swapIV

    let vkCPIn = createVkCommandPoolInfo nullPtr (VkCommandPoolCreateFlags 0) 0
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

    unifor  <- replicateM sCS $ createBuffer vkDev0 [bufferUsageUniformBufferBit] (VkDeviceSize $ 4 * 4 * 2) 1
    let (uniBuf, unifMe) = unzip unifor
    vkDeP0  <- createUniformDescriptorPool vkDev0
    vkDSAI  <- createVkDescriptorSetAllocateInfo nullPtr vkDeP0 3 $ replicate sCS uboDSL
    vkAlDS  <- vkAllocateDescriptorSets vkDev0 vkDSAI
    let uniDBI = map (\x -> VkDescriptorBufferInfo x (VkDeviceSize 0) wholeSize) uniBuf
    vkWDSU  <- mapM (\x -> createVkWriteDescriptorSet nullPtr (vkAlDS !! x) 0 0 1 descriptorTypeUniformBuffer Nothing (Just $ uniDBI !! x) Nothing) [0, 1, 2]
    mapM_ (\x -> vkUpdateDescriptorSets vkDev0 1 (Just [x]) 0 Nothing) vkWDSU

    let zipped = zip vkAlDS vFrame
    vkCoBu  <- mapM (createGraphicsCommandBuffer vkDev0 vertex index vkCPo0 graphP vkPLGr vkRePa indices) zipped

    -- vkBCI   <- createVkBufferInfo nullPtr (VkBufferCreateFlags 0) (VkDeviceSize 2136746240)
    --     [bufferUsageStorageBufferBit, bufferUsageTransferDSTBit] sharingModeExclusive 3 [0]
    -- vkBuff  <- vkCreateBuffer vkDev0 vkBCI
    -- vkBuMR  <- vkGetBufferMemoryRequirements vkDev0 vkBuff
    -- let vkMAI = createVkMemoryAllocateInfo nullPtr (VkDeviceSize $ 2136746240 + 16) 1
    -- vkDeMe  <- vkAllocateMemory vkDev0 vkMAI
    -- buffMa  <- vkMapMemory vkDev0 vkDeMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    -- buffMB  <- vkBindBufferMemory vkDev0 vkBuff vkDeMe (alignment vkBuMR)
    vkICIn  <- createVkImageCreateInfo nullPtr [imageCreateMutableFormatBit] imageType2D formatR16G16B16A16UInt
                (VkExtent3D 256 256 1) 8 1 sampleCount1Bit imageTilingLinear
                [imageUsageColorAttachmentBit, imageUsageTransferDSTBit] sharingModeExclusive 1 [0] imageLayoutUndefined
    vkIma0  <- vkCreateImage vkDev0 vkICIn
    imagMR  <- vkGetImageMemoryRequirements vkDev0 vkIma0
    let imagMI = createVkMemoryAllocateInfo nullPtr (VkDeviceSize $ 699904 + 256) 2
    imagMe  <- vkAllocateMemory vkDev0 imagMI
    imagMa  <- vkMapMemory vkDev0 imagMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    imagMB  <- vkBindImageMemory vkDev0 vkIma0 imagMe (alignment imagMR)
    vkUnmapMemory vkDev0 imagMe
    imagSu  <- createVkImageSubresource [imageAspectColorBit] 4 0
    imagSL  <- vkGetImageSubresourceLayout vkDev0 vkIma0 imagSu

    -- let vkDPS0 = VkDescriptorPoolSize descriptorTypeStorageBuffer 1
    -- vkDPCI <- createVkDescriptorPoolCreateInfo nullPtr (VkDescriptorPoolCreateFlags 0) 1 1 [vkDPS0]
    -- vkDeP0 <- vkCreateDescriptorPool vkDev0 vkDPCI
    -- vkDSAI <- createVkDescriptorSetAllocateInfo nullPtr vkDeP0 1 [vkDSL0]
    -- vkAlDS <- vkAllocateDescriptorSets vkDev0 vkDSAI
    -- let vkDBIn = VkDescriptorBufferInfo vkBuff (VkDeviceSize 0) wholeSize
    -- vkWDS0 <- createVkWriteDescriptorSet nullPtr (head vkAlDS) 0 0 1 descriptorTypeStorageBuffer Nothing (Just vkDBIn) Nothing
    -- vkUpdateDescriptorSets vkDev0 1 (Just [vkWDS0]) 0 Nothing

    -- vkCmdFillBuffer vkCoB0 vkBuff (VkDeviceSize 0) wholeSize 0
    -- vkCmdClearColorImage vkCoB0 vkIma0 imageLayoutGeneral vkCCVa 1 [vkISR0]
    -- vkCmdBindDescriptorSets vkCoB0 pipelineBindPointCompute vkPLCo 0 1 vkAlDS 0 Nothing
    -- vkCmdPushConstants vkCoB0 vkPLCo [shaderStageComputeBit] 0 4 (0 :: Word)
    -- _ <- vkEndCommandBuffer vkCoB0


    return ([color, index, stageC, stageI, stageV, vertex] ++ uniBuf, vkCoBu, vkCPo0, vkDeP0, [uboDSL, vkDSL0], vkDev0, [coloMe, imagMe, indeMe, stagCM, stagIM, stagVM, vertMe], unifMe, fences, vFrame, vkIma0, swapIV, [graphP, compPi], vkPiCa, [vkPLGr, vkPLCo], vkQue0, vkRePa, semaph, vkSC)
    where
        sCS     = 3
        sI      = VkDeviceSize $ 4 * cast (length indices)
        sV      = VkDeviceSize $ 4 * cast vertCo
        vertCo  = fromIntegral (length vertLi) :: Word32
        vertLi  = concatMap vertex3ToList v
        colors  = [0.1, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0]

--------------------------------------------------------------------------------------------------------------------------------
--
-- Shutdown
--
--------------------------------------------------------------------------------------------------------------------------------
shutdown :: [VkBuffer] -> VkCommandPool -> VkDescriptorPool -> [VkDescriptorSetLayout] -> VkDevice -> [VkDeviceMemory] -> [VkFence] -> [VkFramebuffer] -> VkImage -> [VkImageView] -> VkInstance -> [VkPipeline] -> VkPipelineCache -> [VkPipelineLayout] -> VkQueue -> VkRenderPass -> ([VkSemaphore], [VkSemaphore]) -> VkSurfaceKHR -> VkSwapchainKHR -> IO ()
shutdown buffer vkCPo0 vkDeP0 deSLay vkDev0 memory fences vFrame vkIma0 imageV vkInst pipeli vkPiCa pipeLa vkQue0 vkRePa (semaIm, semaPr) vkSurf vkSC = do
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkDeviceWaitIdle vkDev0
    mapM_ (vkDestroyFence vkDev0) fences
    mapM_ (vkDestroySemaphore vkDev0) semaPr
    mapM_ (vkDestroySemaphore vkDev0) semaIm
    mapM_ (vkDestroyFramebuffer vkDev0) vFrame
    vkDestroyRenderPass vkDev0 vkRePa
    vkDestroyDescriptorPool vkDev0 vkDeP0
    vkDestroyPipelineCache vkDev0 vkPiCa
    mapM_ (vkDestroyPipelineLayout vkDev0) pipeLa
    mapM_ (vkDestroyDescriptorSetLayout vkDev0) deSLay
    mapM_ (vkDestroyPipeline vkDev0) pipeli
    vkDestroyCommandPool vkDev0 vkCPo0
    vkDestroyImage vkDev0 vkIma0
    mapM_ (vkDestroyBuffer vkDev0) buffer
    mapM_ (vkFreeMemory vkDev0) memory
    mapM_ (vkDestroyImageView vkDev0) imageV
    vkDestroySwapchainKHR vkDev0 vkSC
    vkDestroyDevice vkDev0
    vkDestroySurfaceKHR vkInst vkSurf
    vkDestroyInstance vkInst
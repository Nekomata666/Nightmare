{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Data.Bits ((.|.))
import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan.Buffers
import Graphics.Vulkan.Command
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data (VkAttachmentDescription(..), VkAttachmentReference(..), VkClearValue(..), VkComponentMapping(..), VkComputePipelineCreateInfo(..), VkDescriptorBufferInfo(..), VkDescriptorPoolSize(..), VkExtent2D(..), VkExtent3D(..), VkImageSubresourceRange(..), VkImageViewCreateInfo(..), VkMemoryRequirements(..), VkOffset2D(..), VkPipelineColorBlendAttachmentState(..), VkPipelineInputAssemblyStateCreateInfo(..), VkPipelineMultisampleStateCreateInfo(..), VkPipelineRasterizationStateCreateInfo(..), VkRect2D(..), VkViewport(..))
import Graphics.Vulkan.Descriptor
import Graphics.Vulkan.Devices
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Framebuffer
import Graphics.Vulkan.Images
import Graphics.Vulkan.Instance
import Graphics.Vulkan.Memory
import Graphics.Vulkan.Pipelines
import Graphics.Vulkan.Queue
import Graphics.Vulkan.Renderpass
import Graphics.Vulkan.Shaders
import Graphics.Vulkan.Surface
import Graphics.Vulkan.Types


createInstance :: IO VkInstance
createInstance = do
    let api = makeAPI 1 2 141
    vkApIn  <- createVkApplicationInfo nullPtr "Nightmare" 0 "Nightmare" 0 api
    vkInCI  <- createVkInstanceCreateInfo nullPtr 0 (Just vkApIn) 1 (Just ["VK_LAYER_KHRONOS_validation"]) 3
        (Just ["VK_EXT_debug_report", "VK_KHR_surface", "VK_KHR_xlib_surface"])
    vkCreateInstance vkInCI

createDevice :: VkInstance -> VkSurfaceKHR -> IO VkDevice
createDevice vkInst vkSurf = do
    vkPDs   <- vkEnumeratePhysicalDevices vkInst
    let vkPD0  = head vkPDs
    vkPDF   <- vkGetPhysicalDeviceFeatures vkPD0
    _       <- vkGetPhysicalDeviceSurfaceSupport vkPD0 0 vkSurf
    vkDQCI  <- createVkDeviceQueueCreateInfo nullPtr (VkDeviceQueueCreateFlags 0) 0 1 [1.0]
    vkDCI   <- createVkDeviceCreateInfo nullPtr (VkDeviceCreateFlags 0) 1 vkDQCI 1 ["VK_KHR_swapchain"] vkPDF
    createVkDevice vkPD0 vkDCI

createGraphicsPipeline :: VkDevice -> VkRenderPass -> IO (VkPipelineLayout, [VkPipeline])
createGraphicsPipeline vkDev0 vkRePa = do
    vkSMIV  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.v.spv"
    vkSMIF  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.f.spv"
    vkSMoV  <- vkCreateShaderModule vkDev0 vkSMIV
    vkSMoF  <- vkCreateShaderModule vkDev0 vkSMIF
    vkPSIV  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageVertexBit vkSMoV "main" Nothing
    vkPSIF  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageFragmentBit vkSMoF "main" Nothing
    vPVICI  <- createVkPipelineVertexInputStateCreateInfo nullPtr (VkPipelineVertexInputStateCreateFlags 0) 0 Nothing 0 Nothing
    let vPISCI = VkPipelineInputAssemblyStateCreateInfo structureTypePipelineInputAssembyStateCreateInfo nullPtr (VkPipelineInputAssemblyStateCreateFlags 0) primitiveTopologyTriangleList (VkBool 0)
        vkView = VkViewport 0 0 1600 900 0 1
        scisso = VkRect2D (VkOffset2D 0 0) (VkExtent2D 1600 900)
    vPVSCI  <- createVkPipelineViewportStateCreateInfo nullPtr (VkPipelineViewportStateCreateFlags 0) 1 [vkView] 1 [scisso]
    let vPRSCI = VkPipelineRasterizationStateCreateInfo structureTypePipelineRasterizationStateCreateInfo nullPtr (VkPipelineRasterizationStateCreateFlags 0) (VkBool 0) (VkBool 0) polygonModeFill (VkCullModeFlags $ unVkCullModeFlagBits cullModeBackBit) frontFaceClockwise (VkBool 0) 0 0 0 1
        vPMSCI = VkPipelineMultisampleStateCreateInfo structureTypePipelineMultisampleStateCreateInfo nullPtr (VkPipelineMultisampleStateCreateFlags 0) sampleCount1Bit (VkBool 0) 1 nullPtr (VkBool 0) (VkBool 0)
        vPCBAS = VkPipelineColorBlendAttachmentState (VkBool 0) blendFactorOne blendFactorZero blendOpAdd blendFactorOne blendFactorZero blendOpAdd colorComponentRGBA
    vPCBCI  <- createVkPipelineColorBlendStateCreateInfo nullPtr (VkPipelineColorBlendStateCreateFlags 0) (VkBool 0) logicOpCopy 1 [vPCBAS] [0,0,0,0]
    vPDSCI  <- createVkPipelineDynamicStateCreateInfo nullPtr (VkPipelineDynamicStateCreateFlags 0) 2 [dynamicStateViewport, dynamicStateLineWidth]
    vkPLCI  <- createVkPipelineLayoutCreateInfo nullPtr (VkPipelineLayoutCreateFlags 0)  0 Nothing 0 Nothing
    vkPiLa  <- vkCreatePipelineLayout vkDev0 vkPLCI

    vkGPCI  <- createVkGraphicsPipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) 2 [vkPSIV, vkPSIF] vPVICI vPISCI Nothing vPVSCI vPRSCI vPMSCI Nothing vPCBCI Nothing vkPiLa vkRePa 0 (VkPipeline nullHandle) (-1)
    graphP <- vkCreateGraphicsPipelines vkDev0 (VkPipelineCache nullHandle) 1 [vkGPCI]

    vkDestroyShaderModule vkDev0 vkSMoV
    vkDestroyShaderModule vkDev0 vkSMoF
    return (vkPiLa, graphP)
    where
        colorComponentRGBA = VkColorComponentFlags $ unVkColorComponentFlagBits colorComponentRBit .|. unVkColorComponentFlagBits colorComponentGBit .|. unVkColorComponentFlagBits colorComponentBBit .|. unVkColorComponentFlagBits colorComponentABit

createRenderpass :: VkDevice -> IO VkRenderPass
createRenderpass vkDev0 = do
    let vkADCo = VkAttachmentDescription (VkAttachmentDescriptionFlags 0) formatB8G8R8A8SRGB sampleCount1Bit attachmentLoadOpClear attachmentStoreOpStore attachmentLoadOpDontCare attachmentStoreOpDontCare imageLayoutUndefined imageLayoutPresentSRCKHR
        vkARCo = VkAttachmentReference 0 imageLayoutColorAttachmentOptimal

    vkSuD0 <- createVkSubpassDescription (VkSubpassDescriptionFlagBits 0) pipelineBindPointGraphics 0 Nothing 1 (Just [vkARCo]) Nothing Nothing 0 Nothing
    vkRPCI <- createVkRenderPassCreateInfo nullPtr (VkRenderPassCreateFlags 0) 1 (Just [vkADCo]) 1 (Just [vkSuD0]) 0 Nothing
    vkCreateRenderPass vkDev0 vkRPCI

initialize :: VkInstance -> VkSurfaceKHR -> IO ()
initialize vkInst vkSurf = do
    vkDev0 <- createDevice vkInst vkSurf
    vkRePa <- createRenderpass vkDev0
    graphs <- createGraphicsPipeline vkDev0 vkRePa
    let vkPLGr = fst graphs
        graphP = head $ snd graphs

    vkSCCI <- createVkSwapchainCreateInfo nullPtr (VkSwapchainCreateFlagsKHR 0) vkSurf 3 formatB8G8R8A8SRGB colorSpaceSRGBNonlinearKHR
                    (VkExtent2D 1600 900) 1 imageUsageColorAttachmentBit sharingModeExclusive 1 [0] surfaceTransformIdentityBitKHR
                    compositeAlphaOpaqueBitKHR presentModeFIFOKHR (VkBool 1) (VkSwapchainKHR nullHandle)
    vkSC    <- vkCreateSwapchainKHR vkDev0 vkSCCI
    vkSCIs  <- vkGetSwapchainImagesKHR vkDev0 vkSC
    let vkIVCI = VkImageViewCreateInfo structureTypeImageViewCreateInfo nullPtr (VkImageViewCreateFlags 0) (head vkSCIs)
                    imageViewType2D formatB8G8R8A8SRGB vkCMId vkISR0
    vkIV0   <- vkCreateImageView vkDev0 vkIVCI

    vkFCI0  <- createVkFramebufferCreateInfo nullPtr (VkFramebufferCreateFlags 0) vkRePa 1 [vkIV0] 1600 900 1
    vkFram  <- vkCreateFramebuffer vkDev0 vkFCI0

    let vkCPIn = createVkCommandPoolInfo nullPtr (VkCommandPoolCreateFlags 0) 0
    vkCPo0  <- vkCreateCommandPool vkDev0 vkCPIn
    let vkCBAI = createVkCommandBufferAllocateInfo nullPtr vkCPo0 commandBufferLevelPrimary 1
    vkCoBu  <- vkAllocateCommandBuffers vkDev0 vkCBAI
    vkCBBI  <- createVkCommandBufferBeginInfo nullPtr (VkCommandBufferUsageFlags 0) Nothing
    let vkCoB0 = head vkCoBu
    _ <- vkBeginCommandBuffer vkCoB0 vkCBBI
    vkCCVa  <- createVkClearColorValue [0,0,0,0]
    let rendAr = VkRect2D (VkOffset2D 0 0) (VkExtent2D 1600 900)
        vkClVa = VkClearValueC vkCCVa
    vkRPBI  <- createVkRenderPassBeginInfo nullPtr vkRePa vkFram rendAr 1 [vkClVa]
    vkCmdBeginRenderPass vkCoB0 vkRPBI subpassContentsInline
    vkCmdBindPipeline vkCoB0 pipelineBindPointGraphics graphP
    vkCmdDraw vkCoB0 3 1 0 0
    vkCmdEndRenderPass vkCoB0

    vkBCI   <- vkCreateBufferInfo nullPtr (VkBufferCreateFlags 0) (VkDeviceSize 2136746240)
        [bufferUsageStorageBufferBit, bufferUsageTransferDSTBit] sharingModeExclusive 3 [0]
    vkBuff  <- vkCreateBuffer vkDev0 vkBCI
    vkBuMR  <- vkGetBufferMemoryRequirements vkDev0 vkBuff
    let vkMAI = vkCreateMemoryAllocateInfo nullPtr (VkDeviceSize $ 2136746240 + 16) 1
    vkDeMe  <- vkAllocateMemory vkDev0 vkMAI
    buffMa  <- vkMapMemory vkDev0 vkDeMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    buffMB  <- vkBindBufferMemory vkDev0 vkBuff vkDeMe (alignment vkBuMR)
    vkICIn  <- createVkImageCreateInfo nullPtr [imageCreateMutableFormatBit] imageType2D formatR16G16B16A16UInt
                (VkExtent3D 256 256 1) 8 1 sampleCount1Bit imageTilingLinear
                [imageUsageColorAttachmentBit, imageUsageTransferDSTBit] sharingModeExclusive 1 [0] imageLayoutUndefined
    vkIma0  <- vkCreateImage vkDev0 vkICIn
    imagMR  <- vkGetImageMemoryRequirements vkDev0 vkIma0
    let imagMI = vkCreateMemoryAllocateInfo nullPtr (VkDeviceSize $ 699904 + 256) 2
    imagMe  <- vkAllocateMemory vkDev0 imagMI
    imagMa  <- vkMapMemory vkDev0 imagMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    imagMB  <- vkBindImageMemory vkDev0 vkIma0 imagMe (alignment imagMR)
    imagSu  <- createVkImageSubresource [imageAspectColorBit] 4 0
    imagSL  <- vkGetImageSubresourceLayout vkDev0 vkIma0 imagSu
    vkSMIC  <- createVkShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.c.spv"
    vkSMoC  <- vkCreateShaderModule vkDev0 vkSMIC
    vkPSIC  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageComputeBit vkSMoC "main" Nothing
    vkDSLB  <- createVkDescriptorSetLayoutBinding 0 descriptorTypeStorageBuffer 1 [shaderStageComputeBit] Nothing
    vDSLCI  <- createVkDescriptorSetLayoutCreateInfo nullPtr (VkDescriptorSetLayoutCreateFlags 0) 1 (Just [vkDSLB])
    vkDSL0  <- vkCreateDescriptorSetLayout vkDev0 vDSLCI
    vkPLCI  <- createVkPipelineLayoutCreateInfo nullPtr (VkPipelineLayoutCreateFlags 0) 1 (Just [vkDSL0]) 0 Nothing
    vkPLCo  <- vkCreatePipelineLayout vkDev0 vkPLCI
    vkPCCI  <- createVkPipelineCacheInfo nullPtr (VkPipelineCacheCreateFlags 0) ""
    vkPiCa  <- vkCreatePipelineCache vkDev0 vkPCCI
    let vkCPCI = VkComputePipelineCreateInfo structureTypeComputePipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) vkPSIC vkPLCo (VkPipeline 0) 0
    vkCoPi <- vkCreateComputePipelines vkDev0 vkPiCa 1 [vkCPCI]
    let vkDPS0 = VkDescriptorPoolSize descriptorTypeStorageBuffer 1
    vkDPCI <- createVkDescriptorPoolCreateInfo nullPtr (VkDescriptorPoolCreateFlags 0) 1 1 [vkDPS0]
    vkDeP0 <- vkCreateDescriptorPool vkDev0 vkDPCI
    vkDSAI <- createVkDescriptorSetAllocateInfo nullPtr vkDeP0 1 [vkDSL0]
    vkAlDS <- vkAllocateDescriptorSets vkDev0 vkDSAI
    let vkDBIn = VkDescriptorBufferInfo vkBuff (VkDeviceSize 0) wholeSize
    vkWDS0 <- createVkWriteDescriptorSet nullPtr (head vkAlDS) 0 0 1 descriptorTypeStorageBuffer Nothing (Just vkDBIn) Nothing
    vkUpdateDescriptorSets vkDev0 1 (Just [vkWDS0]) 0 Nothing
    vkQue0 <- vkGetDeviceQueue vkDev0 0 0

    -- vkCmdFillBuffer vkCoB0 vkBuff (VkDeviceSize 0) wholeSize 0
    -- vkCmdClearColorImage vkCoB0 vkIma0 imageLayoutGeneral vkCCVa 1 [vkISR0]
    let vkCoP0 = head vkCoPi
    vkCmdBindDescriptorSets vkCoB0 pipelineBindPointCompute vkPLCo 0 1 vkAlDS 0 Nothing
    -- vkCmdPushConstants vkCoB0 vkPLCo [shaderStageComputeBit] 0 4 (0 :: Word)
    _ <- vkEndCommandBuffer vkCoB0
    vkSuIn <- createVkSubmitInfo nullPtr 0 Nothing Nothing 1 vkCoBu 0 Nothing
    _ <- vkQueueSubmit vkQue0 1 [vkSuIn] (VkFence nullHandle)


    ----------------------------------------------------------------------------------------------------------------------------
    --
    -- Shutdown
    --
    ----------------------------------------------------------------------------------------------------------------------------
    _ <- vkQueueWaitIdle vkQue0
    _ <- vkDeviceWaitIdle vkDev0
    vkDestroyFramebuffer vkDev0 vkFram
    vkDestroyRenderPass vkDev0 vkRePa
    vkDestroyDescriptorPool vkDev0 vkDeP0
    vkDestroyPipelineCache vkDev0 vkPiCa
    vkDestroyPipelineLayout vkDev0 vkPLGr
    vkDestroyPipeline vkDev0 graphP
    vkDestroyPipelineLayout vkDev0 vkPLCo
    vkDestroyDescriptorSetLayout vkDev0 vkDSL0
    vkDestroyPipeline vkDev0 vkCoP0
    vkDestroyShaderModule vkDev0 vkSMoC
    vkDestroyCommandPool vkDev0 vkCPo0
    vkUnmapMemory vkDev0 imagMe
    vkFreeMemory vkDev0 imagMe
    vkDestroyImage vkDev0 vkIma0
    vkUnmapMemory vkDev0 vkDeMe
    vkFreeMemory vkDev0 vkDeMe
    vkDestroyBuffer vkDev0 vkBuff
    vkDestroyImageView vkDev0 vkIV0
    vkDestroySwapchainKHR vkDev0 vkSC
    vkDestroyDevice vkDev0
    vkDestroySurfaceKHR vkInst vkSurf
    vkDestroyInstance vkInst

    return ()
    where
        vkISR0 = createVkImageSubresourceRange [imageAspectColorBit] 0 1 0 1
        vkCMId = VkComponentMapping componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity

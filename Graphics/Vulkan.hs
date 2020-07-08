{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan.Buffers
import Graphics.Vulkan.Command
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data (VkComputePipelineCreateInfo(..), VkDescriptorBufferInfo(..), VkDescriptorPoolSize(..), VkExtent2D(..), VkExtent3D(..), VkMemoryRequirements(..))
import Graphics.Vulkan.Descriptor
import Graphics.Vulkan.Devices
import Graphics.Vulkan.Enumerations
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

createDevice :: VkInstance -> IO VkDevice
createDevice vkInst = do
    vkPDs   <- vkEnumeratePhysicalDevices vkInst
    let vkPD0  = head vkPDs
    vkPDF   <- vkGetPhysicalDeviceFeatures vkPD0
    vkDQCI  <- createVkDeviceQueueCreateInfo nullPtr (VkDeviceQueueCreateFlags 0) 0 1 [1.0]
    vkDCI   <- createVkDeviceCreateInfo nullPtr (VkDeviceCreateFlags 0) 1 vkDQCI 1 ["VK_KHR_swapchain"] vkPDF
    vkCreateDevice vkPD0 vkDCI

initialize :: VkInstance -> VkSurfaceKHR -> VkDevice -> IO ()
initialize vkInst vkSurf vkDev0 = do
    vkSCCI <- createVkSwapchainCreateInfo nullPtr (VkSwapchainCreateFlagsKHR 0) vkSurf 3 formatB8G8R8A8SRGB colorSpaceSRGBNonlinearKHR
                    (VkExtent2D 1600 900) 1 imageUsageColorAttachmentBit sharingModeExclusive 1 [0] surfaceTransformIdentityBitKHR
                    compositeAlphaOpaqueBitKHR presentModeFIFOKHR (VkBool 1) (VkSwapchainKHR nullHandle)
    vkSC    <- vkCreateSwapchainKHR vkDev0 vkSCCI
    vkBCI   <- vkCreateBufferInfo nullPtr (VkBufferCreateFlags 0) (VkDeviceSize 2136746240)
        [bufferUsageStorageBufferBit, bufferUsageTransferDSTBit] sharingModeExclusive 3 [0]
    vkBuff  <- vkCreateBuffer vkDev0 vkBCI
    vkBuMR  <- vkGetBufferMemoryRequirements vkDev0 vkBuff
    let vkMAI = vkCreateMemoryAllocateInfo nullPtr (VkDeviceSize $ 2136746240 + 16) 1
    vkDeMe  <- vkAllocateMemory vkDev0 vkMAI
    buffMa  <- vkMapMemory vkDev0 vkDeMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    buffMB  <- vkBindBufferMemory vkDev0 vkBuff vkDeMe (alignment vkBuMR)
    imInfo  <- vkCreateImageInfo nullPtr [imageCreateMutableFormatBit] imageType2D formatR16G16B16A16UInt
                (VkExtent3D 256 256 1) 8 1 sampleCount1Bit imageTilingLinear
                [imageUsageColorAttachmentBit, imageUsageTransferDSTBit] sharingModeExclusive 1 [0] imageLayoutUndefined
    vkIma0  <- vkCreateImage vkDev0 imInfo
    imagMR  <- vkGetImageMemoryRequirements vkDev0 vkIma0
    let imagMI = vkCreateMemoryAllocateInfo nullPtr (VkDeviceSize $ 699904 + 256) 2
    imagMe  <- vkAllocateMemory vkDev0 imagMI
    imagMa  <- vkMapMemory vkDev0 imagMe (VkDeviceSize 0) wholeSize (VkMemoryMapFlags 0)
    imagMB  <- vkBindImageMemory vkDev0 vkIma0 imagMe (alignment imagMR)
    imagSu  <- vkCreateImageSubresource [imageAspectColorBit] 4 0
    imagSL  <- vkGetImageSubresourceLayout vkDev0 vkIma0 imagSu
    vkCCVa  <- createVkClearColorValue [0,0,0,0]
    vkSMIn  <- createShaderModuleInfo nullPtr (VkShaderModuleCreateFlags 0) "Shaders/Simple.spv"
    vkSMod  <- vkCreateShaderModule vkDev0 vkSMIn
    vkPSSI  <- createVkPipelineShaderStageInfo nullPtr (VkPipelineShaderStageCreateFlags 0) shaderStageComputeBit vkSMod "main" Nothing
    vkDSLB  <- createVkDescriptorSetLayoutBinding 0 descriptorTypeStorageBuffer 1 [shaderStageComputeBit] Nothing
    vDSLCI  <- createVkDescriptorSetLayoutCreateInfo nullPtr (VkDescriptorSetLayoutCreateFlags 0) 1 (Just [vkDSLB])
    vkDSL0  <- vkCreateDescriptorSetLayout vkDev0 vDSLCI
    vkPLCI  <- createVkPipelineLayoutCreateInfo nullPtr (VkPipelineLayoutCreateFlags 0) 1 [vkDSL0] 0 Nothing
    vkPiLa  <- vkCreatePipelineLayout vkDev0 vkPLCI
    vkPCCI  <- createVkPipelineCacheInfo nullPtr (VkPipelineCacheCreateFlags 0) ""
    vkPiCa  <- vkCreatePipelineCache vkDev0 vkPCCI
    let vkCPCI = VkComputePipelineCreateInfo structureTypeComputePipelineCreateInfo nullPtr (VkPipelineCreateFlags 0) vkPSSI vkPiLa (VkPipeline 0) 0
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
    vkSuD0 <- createVkSubpassDescription (VkSubpassDescriptionFlagBits 0) pipelineBindPointGraphics 0 Nothing 0 Nothing Nothing Nothing 0 Nothing
    vkRPCI <- createVkRenderPassCreateInfo nullPtr (VkRenderPassCreateFlags 0) 0 Nothing 1 (Just [vkSuD0]) 0 Nothing
    vkRePa <- vkCreateRenderPass vkDev0 vkRPCI
    let vkCPIn = createVkCommandPoolInfo nullPtr (VkCommandPoolCreateFlags 0) 0
    vkCPo0 <- vkCreateCommandPool vkDev0 vkCPIn
    let vkCBAI = createVkCommandBufferAllocateInfo nullPtr vkCPo0 commandBufferLevelPrimary 1
    vkCoBu <- vkAllocateCommandBuffers vkDev0 vkCBAI
    vkCBBI <- createVkCommandBufferBeginInfo nullPtr (VkCommandBufferUsageFlags 0) Nothing
    let vkCoB0 = head vkCoBu
    _ <- vkBeginCommandBuffer vkCoB0 vkCBBI
    vkCmdFillBuffer vkCoB0 vkBuff (VkDeviceSize 0) wholeSize 0
    vkCmdClearColorImage vkCoB0 vkIma0 imageLayoutGeneral vkCCVa 1 [vkISR0]
    let vkCoP0 = head vkCoPi
    vkCmdBindPipeline vkCoB0 pipelineBindPointCompute vkCoP0
    vkCmdBindDescriptorSets vkCoB0 pipelineBindPointCompute vkPiLa 0 1 vkAlDS 0 Nothing
    -- vkCmdPushConstants vkCoB0 vkPiLa [shaderStageComputeBit] 0 4 (0 :: Word)
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
    vkDestroyRenderPass vkDev0 vkRePa
    vkDestroyDescriptorPool vkDev0 vkDeP0
    vkDestroyPipelineCache vkDev0 vkPiCa
    vkDestroyPipelineLayout vkDev0 vkPiLa
    vkDestroyDescriptorSetLayout vkDev0 vkDSL0
    vkDestroyPipeline vkDev0 vkCoP0
    vkDestroyShaderModule vkDev0 vkSMod
    vkDestroyCommandPool vkDev0 vkCPo0
    vkUnmapMemory vkDev0 imagMe
    vkFreeMemory vkDev0 imagMe
    vkDestroyImage vkDev0 vkIma0
    vkUnmapMemory vkDev0 vkDeMe
    vkFreeMemory vkDev0 vkDeMe
    vkDestroyBuffer vkDev0 vkBuff
    vkDestroyDevice vkDev0
    vkDestroyInstance vkInst

    return ()
    where
        vkISR0 = createVkImageSubresourceRange [imageAspectColorBit] 0 1 0 1

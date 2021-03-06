{-# LANGUAGE DuplicateRecordFields, Safe #-}

module Graphics.Vulkan.Data where


import Data.Void (Void)
import Data.Word (Word8, Word32, Word64)

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


data VkAllocationCallbacks = VkAllocationCallbacks{
    userData                :: Ptr Void,
    pfnAllocation           :: PFN_vkAllocationFunction,
    pfnReallocation         :: PFN_vkReallocationFunction,
    pfnFree                 :: PFN_vkFreeFunction,
    pfnInternalAllocation   :: PFN_vkInternalAllocationNotification,
    pfnInternalFree         :: PFN_vkInternalFreeNotification
}

data VkApplicationInfo = VkApplicationInfo{
    sType               :: VkStructureType,
    next                :: Ptr Void,
    applicationName     :: CString,
    applicationVersion  :: Word32,
    engineName          :: CString,
    engineVersion       :: Word32,
    apiVersion          :: Word32
}

data VkAttachmentDescription = VkAttachmentDescription{
    flags :: VkAttachmentDescriptionFlags,
    format :: VkFormat,
    samples :: VkSampleCountFlagBits,
    loadOp :: VkAttachmentLoadOp,
    storeOp :: VkAttachmentStoreOp,
    stencilLoadOp :: VkAttachmentLoadOp,
    stencilStoreOp :: VkAttachmentStoreOp,
    initialLayout :: VkImageLayout,
    finalLayout :: VkImageLayout
}

data VkAttachmentReference = VkAttachmentReference{
    attachment :: Word32,
    layout :: VkImageLayout
}

data VkBufferCreateInfo = VkBufferCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkBufferCreateFlags,
    size :: VkDeviceSize,
    usage :: VkBufferUsageFlags,
    sharingMode :: VkSharingMode,
    queueFamilyIndexCount :: Word32,
    queueFamilyIndices :: Ptr Word32
}

-- Note: At most, we can add Int32.
-- Using only Word32s!
data VkClearColorValue = VkClearColorValue{
    word32 :: Ptr Word32 -- [4]
}

data VkClearDepthStencilValue = VkClearDepthStencilValue{
    depth :: Float,
    stencil :: Word32
}

data VkClearValue = VkClearValueC{color :: VkClearColorValue} |
    VkClearValueDS{depthStencil :: VkClearDepthStencilValue}

data VkCommandBufferAllocateInfo =VkCommandBufferAllocateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    commandPool :: VkCommandPool,
    level :: VkCommandBufferLevel,
    commandBufferCount :: Word32
}

data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkCommandBufferUsageFlags,
    pInheritanceInfo :: Ptr VkCommandBufferInheritanceInfo
}

data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    renderPass :: VkRenderPass,
    subpass :: Word32,
    framebuffer :: VkFramebuffer,
    occlusionQueryEnable :: VkBool,
    queryFlags :: VkQueryControlFlags,
    pipelineStatistics :: VkQueryPipelineStatisticFlags
}

data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkCommandPoolCreateFlags,
    queueFamilyIndex :: Word32
}

data VkComponentMapping = VkComponentMapping{
    red :: VkComponentSwizzle,
    green :: VkComponentSwizzle,
    blue :: VkComponentSwizzle,
    alpha :: VkComponentSwizzle
}

data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineCreateFlags,
    stage :: VkPipelineShaderStageCreateInfo,
    layout :: VkPipelineLayout,
    basePipelineHandle :: VkPipeline,
    basePipelineIndex :: Int32
}

data VkCopyDescriptorSet = VkCopyDescriptorSet{
    sType :: VkStructureType,
    next :: Ptr Void,
    srcSet :: VkDescriptorSet,
    srcBinding :: Word32,
    srcArrayElement :: Word32,
    dstSet :: VkDescriptorSet,
    dstBinding :: Word32,
    dstArrayElement :: Word32,
    descriptorCount :: Word32
}

data VkDescriptorBufferInfo = VkDescriptorBufferInfo{
    buffer :: VkBuffer,
    offset :: VkDeviceSize,
    range :: VkDeviceSize
}

data VkDescriptorImageInfo = VkDescriptorImageInfo{
    sampler :: VkSampler,
    imageView :: VkImageView,
    imageLayout :: VkImageLayout
}

data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkDescriptorPoolCreateFlags,
    maxSets :: Word32,
    poolSizeCount :: Word32,
    pPoolSizes :: Ptr VkDescriptorPoolSize
}

data VkDescriptorPoolSize = VkDescriptorPoolSize{
    dType :: VkDescriptorType,
    descriptorCount :: Word32
}

data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    descriptorPool :: VkDescriptorPool,
    descriptorSetCount :: Word32,
    pSetLayouts :: Ptr VkDescriptorSetLayout
}

data VkDescriptorSetLayoutBinding = VkDescriptorSetLayoutBinding{
    binding :: Word32,
    descriptorType :: VkDescriptorType,
    descriptorCount :: Word32,
    stageFlags :: VkShaderStageFlags,
    pImmutableSamplers :: Ptr VkSampler
}

data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkDescriptorSetLayoutCreateFlags,
    bindingCount :: Word32,
    pBindings :: Ptr VkDescriptorSetLayoutBinding
}

data VkDeviceCreateInfo = VkDeviceCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkDeviceCreateFlags,
    queueCreateInfoCount :: Word32,
    queueCreateInfos :: Ptr VkDeviceQueueCreateInfo,
    enabledLayerCount :: Word32,
    enabledLayerNames :: Ptr CString,
    enabledExtensionCount :: Word32,
    enabledExtensionNames :: Ptr CString,
    enabledFeatures :: Ptr VkPhysicalDeviceFeatures
}

data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkDeviceQueueCreateFlags,
    queueFamilyIndex :: Word32,
    queueCount :: Word32,
    queuePriorities :: Ptr Float
}

data VkExtent2D = VkExtent2D{
    width :: Word32,
    height :: Word32
}

data VkExtent3D = VkExtent3D{
    width :: Word32,
    height :: Word32,
    depth :: Word32
}

data VkFenceCreateInfo = VkFenceCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkFenceCreateFlags
}

data VkFramebufferCreateInfo = VkFramebufferCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkFramebufferCreateFlags,
    renderPass :: VkRenderPass,
    attachmentCount :: Word32,
    pAttachments :: Ptr VkImageView,
    width :: Word32,
    height :: Word32,
    layers :: Word32
}

data VkGraphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineCreateFlags,
    stageCount :: Word32,
    pStages :: Ptr VkPipelineShaderStageCreateInfo,
    pVertexInputState :: Ptr VkPipelineVertexInputStateCreateInfo,
    pInputAssemblyState :: Ptr VkPipelineInputAssemblyStateCreateInfo,
    pTessellationState :: Ptr VkPipelineTessellationStateCreateInfo,
    pViewportState :: Ptr VkPipelineViewportStateCreateInfo,
    pRasterizationState :: Ptr VkPipelineRasterizationStateCreateInfo,
    pMultisampleState :: Ptr VkPipelineMultisampleStateCreateInfo,
    pDepthStencilState :: Ptr VkPipelineDepthStencilStateCreateInfo,
    pColorBlendState :: Ptr VkPipelineColorBlendStateCreateInfo,
    pDynamicState :: Ptr VkPipelineDynamicStateCreateInfo,
    layout :: VkPipelineLayout,
    renderPass :: VkRenderPass,
    subpass :: Word32,
    basePipelineHandle :: VkPipeline,
    basePipelineIndex :: Int32
}

data VkImageCreateInfo = VkImageCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkImageCreateFlags,
    imageType :: VkImageType,
    format :: VkFormat,
    extent :: VkExtent3D,
    mipLevels :: Word32,
    arrayLayers :: Word32,
    samples :: VkSampleCountFlagBits,
    tiling :: VkImageTiling,
    usage :: VkImageUsageFlags,
    sharingMode :: VkSharingMode,
    queueFamilyIndexCount :: Word32,
    queueFamilyIndices :: Ptr Word32,
    initialLayout :: VkImageLayout
}

data VkImageSubresource = VkImageSubresource{
    aspectMask :: VkImageAspectFlags,
    mipLevel :: Word32,
    arrayLayer :: Word32
}

data VkImageSubresourceRange = VkImageSubresourceRange{
    aspectMask :: VkImageAspectFlags,
    baseMipLevel :: Word32,
    levelCount :: Word32,
    baseArrayLayer :: Word32,
    layerCount :: Word32
}

data VkImageViewCreateInfo = VkImageViewCreateInfo{
    sType               :: VkStructureType,
    next                :: Ptr Void,
    flags               :: VkImageViewCreateFlags,
    image               :: VkImage,
    viewType            :: VkImageViewType,
    format              :: VkFormat,
    components          :: VkComponentMapping,
    subresourceRange    :: VkImageSubresourceRange
}

data VkInstanceCreateInfo = VkInstanceCreateInfo{
    sType                   :: VkStructureType,
    next                    :: Ptr Void,
    flags                   :: VkFlags,
    applicationInfo         :: Ptr VkApplicationInfo,
    enabledLayerCount       :: Word32,
    enabledLayerNames       :: Ptr CString,
    enabledExtensionCount   :: Word32,
    enabledExtensionNames   :: Ptr CString
}

data VkMemoryAllocateInfo = VkMemoryAllocateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    allocationSize :: VkDeviceSize,
    memoryTypeIndex :: Word32
}

data VkMemoryRequirements = VkMemoryRequirements{
    size :: VkDeviceSize,
    alignment :: VkDeviceSize,
    memoryTypeBits :: Word32
}

data VkOffset2D = VkOffset2D{
    x :: Int32,
    y :: Int32
}

data VkPhysicalDeviceFeatures = VkPhysicalDeviceFeatures{
    robustBufferAccess :: VkBool,
    fullDrawIndexUint32 :: VkBool,
    imageCubeArray :: VkBool,
    independentBlend :: VkBool,
    geometryShader :: VkBool,
    tessellationShader :: VkBool,
    sampleRateShading :: VkBool,
    dualSrcBlend :: VkBool,
    logicOp :: VkBool,
    multiDrawIndirect :: VkBool,
    drawIndirectFirstInstance :: VkBool,
    depthClamp :: VkBool,
    depthBiasClamp :: VkBool,
    fillModeNonSolid :: VkBool,
    depthBounds :: VkBool,
    wideLines :: VkBool,
    largePoints :: VkBool,
    alphaToOne :: VkBool,
    multiViewport :: VkBool,
    samplerAnisotropy :: VkBool,
    textureCompressionETC2 :: VkBool,
    textureCompressionASTC_LDR :: VkBool,
    textureCompressionBC :: VkBool,
    occlusionQueryPrecise :: VkBool,
    pipelineStatisticsQuery :: VkBool,
    vertexPipelineStoresAndAtomics :: VkBool,
    fragmentStoresAndAtomics :: VkBool,
    shaderTessellationAndGeometryPointSize :: VkBool,
    shaderImageGatherExtended :: VkBool,
    shaderStorageImageExtendedFormats :: VkBool,
    shaderStorageImageMultisample :: VkBool,
    shaderStorageImageReadWithoutFormat :: VkBool,
    shaderStorageImageWriteWithoutFormat :: VkBool,
    shaderUniformBufferArrayDynamicIndexing :: VkBool,
    shaderSampledImageArrayDynamicIndexing :: VkBool,
    shaderStorageBufferArrayDynamicIndexing :: VkBool,
    shaderStorageImageArrayDynamicIndexing :: VkBool,
    shaderClipDistance :: VkBool,
    shaderCullDistance :: VkBool,
    shaderFloat64 :: VkBool,
    shaderInt64 :: VkBool,
    shaderInt16 :: VkBool,
    shaderResourceResidency :: VkBool,
    shaderResourceMinLod :: VkBool,
    sparseBinding :: VkBool,
    sparseResidencyBuffer :: VkBool,
    sparseResidencyImage2D :: VkBool,
    sparseResidencyImage3D :: VkBool,
    sparseResidency2Samples :: VkBool,
    sparseResidency4Samples :: VkBool,
    sparseResidency8Samples :: VkBool,
    sparseResidency16Samples :: VkBool,
    sparseResidencyAliased :: VkBool,
    variableMultisampleRate :: VkBool,
    inheritedQueries :: VkBool
} deriving (Show)

data VkPhysicalDeviceFeatures2 = VkPhysicalDeviceFeatures2 {
    sType :: VkStructureType,
    pNext :: Ptr Void,
    features :: VkPhysicalDeviceFeatures
} deriving (Show)

data VkPhysicalDeviceVulkan11Features = VkPhysicalDeviceVulkan11Features {
    sType :: VkStructureType,
    pNext :: Ptr Void,
    storageBuffer16BitAccess :: VkBool,
    uniformAndStorageBuffer16BitAccess :: VkBool,
    storagePushConstant16 :: VkBool,
    storageInputOutput16 :: VkBool,
    multiview :: VkBool,
    multiviewGeometryShader :: VkBool,
    multiviewTessellationShader :: VkBool,
    variablePointersStorageBuffer :: VkBool,
    variablePointers :: VkBool,
    protectedMemory :: VkBool,
    samplerYcbcrConversion :: VkBool,
    shaderDrawParameters :: VkBool
} deriving (Show)

data VkPhysicalDeviceVulkan12Features = VkPhysicalDeviceVulkan12Features {
    sType :: VkStructureType,
    pNext :: Ptr Void,
    samplerMirrorClampToEdge :: VkBool,
    drawIndirectCount :: VkBool,
    storageBuffer8BitAccess :: VkBool,
    uniformAndStorageBuffer8BitAccess :: VkBool,
    storagePushConstant8 :: VkBool,
    shaderBufferInt64Atomics :: VkBool,
    shaderSharedInt64Atomics :: VkBool,
    shaderFloat16 :: VkBool,
    shaderInt8 :: VkBool,
    descriptorIndexing :: VkBool,
    shaderInputAttachmentArrayDynamicIndexing :: VkBool,
    shaderUniformTexelBufferArrayDynamicIndexing :: VkBool,
    shaderStorageTexelBufferArrayDynamicIndexing :: VkBool,
    shaderUniformBufferArrayNonUniformIndexing :: VkBool,
    shaderSampledImageArrayNonUniformIndexing :: VkBool,
    shaderStorageBufferArrayNonUniformIndexing :: VkBool,
    shaderStorageImageArrayNonUniformIndexing :: VkBool,
    shaderInputAttachmentArrayNonUniformIndexing :: VkBool,
    shaderUniformTexelBufferArrayNonUniformIndexing :: VkBool,
    shaderStorageTexelBufferArrayNonUniformIndexing :: VkBool,
    descriptorBindingUniformBufferUpdateAfterBind :: VkBool,
    descriptorBindingSampledImageUpdateAfterBind :: VkBool,
    descriptorBindingStorageImageUpdateAfterBind :: VkBool,
    descriptorBindingStorageBufferUpdateAfterBind :: VkBool,
    descriptorBindingUniformTexelBufferUpdateAfterBind :: VkBool,
    descriptorBindingStorageTexelBufferUpdateAfterBind :: VkBool,
    descriptorBindingUpdateUnusedWhilePending :: VkBool,
    descriptorBindingPartiallyBound :: VkBool,
    descriptorBindingVariableDescriptorCount :: VkBool,
    runtimeDescriptorArray :: VkBool,
    samplerFilterMinmax :: VkBool,
    scalarBlockLayout :: VkBool,
    imagelessFramebuffer :: VkBool,
    uniformBufferStandardLayout :: VkBool,
    shaderSubgroupExtendedTypes :: VkBool,
    separateDepthStencilLayouts :: VkBool,
    hostQueryReset :: VkBool,
    timelineSemaphore :: VkBool,
    bufferDeviceAddress :: VkBool,
    bufferDeviceAddressCaptureReplay :: VkBool,
    bufferDeviceAddressMultiDevice :: VkBool,
    vulkanMemoryModel :: VkBool,
    vulkanMemoryModelDeviceScope :: VkBool,
    vulkanMemoryModelAvailabilityVisibilityChains :: VkBool,
    shaderOutputViewportIndex :: VkBool,
    shaderOutputLayer :: VkBool,
    subgroupBroadcastDynamicId :: VkBool
} deriving (Show)

data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineCacheCreateFlags,
    initialDataSize :: CSize,
    pInitialData :: Ptr Void
}

data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState{
    blendEnable :: VkBool,
    srcColorBlendFactor :: VkBlendFactor,
    dstColorBlendFactor :: VkBlendFactor,
    colorBlendOp :: VkBlendOp,
    srcAlphaBlendFactor :: VkBlendFactor,
    dstAlphaBlendFactor :: VkBlendFactor,
    alphaBlendOp :: VkBlendOp,
    colorWriteMask :: VkColorComponentFlags
}

data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineColorBlendStateCreateFlags,
    logicOpEnable :: VkBool,
    logicOp :: VkLogicOp,
    attachmentCount :: Word32,
    pAttachments :: Ptr VkPipelineColorBlendAttachmentState,
    blendConstants :: Ptr Float -- [4]
}

data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineDepthStencilStateCreateFlags,
    depthTestEnable :: VkBool,
    depthWriteEnable :: VkBool,
    depthCompareOp :: VkCompareOp,
    depthBoundsTestEnable :: VkBool,
    stencilTestEnable :: VkBool,
    front :: VkStencilOpState,
    back :: VkStencilOpState,
    minDepthBounds :: Float,
    maxDepthBounds :: Float
}

data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineDynamicStateCreateFlags,
    dynamicStateCount :: Word32,
    pDynamicStates :: Ptr VkDynamicState
}

data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineInputAssemblyStateCreateFlags,
    topology :: VkPrimitiveTopology,
    primitiveRestartEnable :: VkBool
}

data VkPipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineLayoutCreateFlags,
    setLayoutCount :: Word32,
    pSetLayouts :: Ptr VkDescriptorSetLayout,
    pushConstantRangeCount :: Word32,
    pPushConstantRanges :: Ptr VkPushConstantRange
}

data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineMultisampleStateCreateFlags,
    rasterizationSamples :: VkSampleCountFlagBits,
    sampleShadingEnable :: VkBool,
    minSampleShading :: Float,
    pSampleMask :: Ptr VkSampleMask,
    alphaToCoverageEnable :: VkBool,
    alphaToOneEnable :: VkBool
}

data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineRasterizationStateCreateFlags,
    depthClampEnable :: VkBool,
    rasterizerDiscardEnable :: VkBool,
    polygonMode :: VkPolygonMode,
    cullMode :: VkCullModeFlags,
    frontFace :: VkFrontFace,
    depthBiasEnable :: VkBool,
    depthBiasConstantFactor :: Float,
    depthBiasClamp :: Float,
    depthBiasSlopeFactor :: Float,
    lineWidth :: Float
}

data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineShaderStageCreateFlags,
    stage :: VkShaderStageFlagBits,
    vkModule :: VkShaderModule,
    pName :: CString,
    pSpecializationInfo :: Ptr VkSpecializationInfo
}

data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineTessellationStateCreateFlags,
    patchControlPoints :: Word32
}

data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineVertexInputStateCreateFlags,
    vertexBindingDescriptionCount :: Word32,
    pVertexBindingDescriptions :: Ptr VkVertexInputBindingDescription,
    vertexAttributeDescriptionCount :: Word32,
    pVertexAttributeDescriptions :: Ptr VkVertexInputAttributeDescription
}

data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineViewportStateCreateFlags,
    viewportCount :: Word32,
    pViewports :: Ptr VkViewport,
    scissorCount :: Word32,
    pScissors :: Ptr VkRect2D
}

data VkPresentInfoKHR = VkPresentInfoKHR{
    sType :: VkStructureType,
    next :: Ptr Void,
    waitSemaphoreCount :: Word32,
    pWaitSemaphores :: Ptr VkSemaphore,
    swapchainCount :: Word32,
    pSwapchains :: Ptr VkSwapchainKHR,
    pImageIndices :: Ptr Word32,
    pResults :: Ptr VkResult
}

data VkPushConstantRange = VkPushConstantRange{
    stageFlags :: VkShaderStageFlags,
    offset :: Word32,
    size :: Word32
}

data VkRect2D = VkRect2D{
    offset :: VkOffset2D,
    extent :: VkExtent2D
}

data VkRenderPassBeginInfo = VkRenderPassBeginInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    renderPass :: VkRenderPass,
    framebuffer :: VkFramebuffer,
    renderArea :: VkRect2D,
    clearValueCount :: Word32,
    pClearValues :: Ptr VkClearValue
}

data VkRenderPassCreateInfo = VkRenderPassCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkRenderPassCreateFlags,
    attachmentCount :: Word32,
    pAttachments :: Ptr VkAttachmentDescription,
    subpassCount :: Word32,
    pSubpasses :: Ptr VkSubpassDescription,
    dependencyCount :: Word32,
    pDependencies :: Ptr VkSubpassDependency
}

data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkSemaphoreCreateFlags
}

data VkSemaphoreTypeCreateInfo = VkSemaphoreTypeCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    semaphoreType :: VkSemaphoreType,
    initialValue :: Word64
}

data VkShaderModuleCreateInfo = VkShaderModuleCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkShaderModuleCreateFlags,
    codeSize :: CSize,
    pCode :: Ptr Word32
}

data VkSpecializationInfo = VkSpecializationInfo{
    mapEntryCount :: Word32,
    pMapEntries :: Ptr VkSpecializationMapEntry,
    dataSize :: CSize,
    pData :: Ptr Void
}

data VkSpecializationMapEntry = VkSpecializationMapEntry{
    constantID :: Word32,
    offset :: Word32,
    size :: CSize
}

data VkStencilOpState = VkStencilOpState{
    failOp :: VkStencilOp,
    passOp :: VkStencilOp,
    depthFailOp :: VkStencilOp,
    compareOp :: VkCompareOp,
    compareMask :: Word32,
    writeMask :: Word32,
    reference :: Word32
}

data VkSubmitInfo = VkSubmitInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    waitSemaphoreCount :: Word32,
    pWaitSemaphores :: Ptr VkSemaphore,
    pWaitDstStageMask :: Ptr VkPipelineStageFlags,
    commandBufferCount :: Word32,
    pCommandBuffers :: Ptr VkCommandBuffer,
    signalSemaphoreCount :: Word32,
    pSignalSemaphores :: Ptr VkSemaphore
}

data VkSubpassDependency = VkSubpassDependency{
    srcSubpass :: Word32,
    dstSubpass :: Word32,
    srcStageMask :: VkPipelineStageFlags,
    dstStageMask :: VkPipelineStageFlags,
    srcAccessMask :: VkAccessFlags,
    dstAccessMask :: VkAccessFlags,
    dependencyFlags :: VkDependencyFlags
}

data VkSubpassDescription = VkSubpassDescription{
    flags :: VkSubpassDescriptionFlags,
    pipelineBindPoint :: VkPipelineBindPoint,
    inputAttachmentCount :: Word32,
    pInputAttachments :: Ptr VkAttachmentReference,
    colorAttachmentCount :: Word32,
    pColorAttachments :: Ptr VkAttachmentReference,
    pResolveAttachments :: Ptr VkAttachmentReference,
    pDepthStencilAttachment :: Ptr VkAttachmentReference,
    preserveAttachmentCount :: Word32,
    pPreserveAttachments :: Ptr Word32
}

data VkSubresourceLayout = VkSubresourceLayout{
    offset :: VkDeviceSize,
    size :: VkDeviceSize,
    rowPitch :: VkDeviceSize,
    arrayPitch :: VkDeviceSize,
    depthPitch :: VkDeviceSize
}

data VkSwapchainCreateInfoKHR = VkSwapchainCreateInfoKHR{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkSwapchainCreateFlagsKHR,
    surface :: VkSurfaceKHR,
    minImageCount :: Word32,
    imageFormat :: VkFormat,
    imageColorSpace :: VkColorSpaceKHR,
    imageExtent :: VkExtent2D,
    imageArrayLayers :: Word32,
    imageUsage :: VkImageUsageFlags,
    imageSharingMode :: VkSharingMode,
    queueFamilyIndexCount :: Word32,
    queueFamilyIndices :: Ptr Word32,
    preTransform :: VkSurfaceTransformFlagBitsKHR,
    compositeAlpha :: VkCompositeAlphaFlagBitsKHR,
    presentMode :: VkPresentModeKHR,
    clipped :: VkBool,
    oldSwapchain :: VkSwapchainKHR
}

data VkVertexInputAttributeDescription = VkVertexInputAttributeDescription{
    location :: Word32,
    binding :: Word32,
    format :: VkFormat,
    offset :: Word32
}

data VkVertexInputBindingDescription = VkVertexInputBindingDescription{
    binding :: Word32,
    stride :: Word32,
    inputRate :: VkVertexInputRate
}

data VkViewport = VkViewport{
    x :: Float,
    y :: Float,
    width :: Float,
    height :: Float,
    minDepth :: Float,
    maxDepth :: Float
}

data VkWriteDescriptorSet = VkWriteDescriptorSet{
    sType :: VkStructureType,
    next :: Ptr Void,
    dstSet :: VkDescriptorSet,
    dstBinding :: Word32,
    dstArrayElement :: Word32,
    descriptorCount :: Word32,
    descriptorType :: VkDescriptorType,
    pImageInfo :: Ptr VkDescriptorImageInfo,
    pBufferInfo :: Ptr VkDescriptorBufferInfo,
    pTexelBufferView :: Ptr VkBufferView
}

-- Storable instances
instance Storable VkAllocationCallbacks where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        return (VkAllocationCallbacks v1 v2 v3 v4 v5 v6)
    poke p (VkAllocationCallbacks v1 v2 v3 v4 v5 v6) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6

instance Storable VkApplicationInfo where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 44
        return (VkApplicationInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkApplicationInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 44 v7

instance Storable VkAttachmentDescription where
    sizeOf _    = 36
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        v6 <- peekByteOff p 20
        v7 <- peekByteOff p 24
        v8 <- peekByteOff p 28
        v9 <- peekByteOff p 32
        return (VkAttachmentDescription v1 v2 v3 v4 v5 v6 v7 v8 v9)
    poke p (VkAttachmentDescription v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5
        pokeByteOff p 20 v6
        pokeByteOff p 24 v7
        pokeByteOff p 28 v8
        pokeByteOff p 32 v9

instance Storable VkAttachmentReference where
    sizeOf _    = 8
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        return (VkAttachmentReference v1 v2)
    poke p (VkAttachmentReference v1 v2) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2

instance Storable VkBufferCreateInfo where
    sizeOf _    = 56
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 36
        v7 <- peekByteOff p 40
        v8 <- peekByteOff p 48
        return (VkBufferCreateInfo v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkBufferCreateInfo v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 36 v6
        pokeByteOff p 40 v7
        pokeByteOff p 48 v8

instance Storable VkClearColorValue where
    sizeOf _    = 16
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkClearColorValue v)
    poke p (VkClearColorValue v) = pokeByteOff p 0 v

instance Storable VkClearDepthStencilValue where
    sizeOf _    = 8
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        return (VkClearDepthStencilValue v1 v2)
    poke p (VkClearDepthStencilValue v1 v2) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2

instance Storable VkClearValue where
    sizeOf _    = 16
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkClearValueC v)
    peek p = do
        v <- peekByteOff p 0
        return (VkClearValueDS v)
    poke p (VkClearValueC v) = pokeByteOff p 0 v
    poke p (VkClearValueDS v) = pokeByteOff p 0 v

instance Storable VkCommandBufferAllocateInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 28
        return (VkCommandBufferAllocateInfo v1 v2 v3 v4 v5)
    poke p (VkCommandBufferAllocateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 28 v5

instance Storable VkCommandBufferBeginInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        return (VkCommandBufferBeginInfo v1 v2 v3 v4)
    poke p (VkCommandBufferBeginInfo v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4

instance Storable VkCommandBufferInheritanceInfo where
    sizeOf _    = 56
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 44
        v8 <- peekByteOff p 48
        return (VkCommandBufferInheritanceInfo v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkCommandBufferInheritanceInfo v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 44 v7
        pokeByteOff p 48 v8

instance Storable VkCommandPoolCreateInfo where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        return (VkCommandPoolCreateInfo v1 v2 v3 v4)
    poke p (VkCommandPoolCreateInfo v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4

instance Storable VkComponentMapping where
    sizeOf _    = 16
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        return (VkComponentMapping v1 v2 v3 v4)
    poke p (VkComponentMapping v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4

instance Storable VkComputePipelineCreateInfo where
    sizeOf _    = 96
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 72
        v6 <- peekByteOff p 80
        v7 <- peekByteOff p 88
        return (VkComputePipelineCreateInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkComputePipelineCreateInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 72 v5
        pokeByteOff p 80 v6
        pokeByteOff p 88 v7

instance Storable VkCopyDescriptorSet where
    sizeOf _    = 56
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 28
        v6 <- peekByteOff p 32
        v7 <- peekByteOff p 40
        v8 <- peekByteOff p 44
        v9 <- peekByteOff p 48
        return (VkCopyDescriptorSet v1 v2 v3 v4 v5 v6 v7 v8 v9)
    poke p (VkCopyDescriptorSet v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 28 v5
        pokeByteOff p 32 v6
        pokeByteOff p 40 v7
        pokeByteOff p 44 v8
        pokeByteOff p 48 v9

instance Storable VkDescriptorBufferInfo where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        return (VkDescriptorBufferInfo v1 v2 v3)
    poke p (VkDescriptorBufferInfo v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3

instance Storable VkDescriptorImageInfo where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        return (VkDescriptorImageInfo v1 v2 v3)
    poke p (VkDescriptorImageInfo v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3

instance Storable VkDescriptorPoolCreateInfo where
    sizeOf _    = 40
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        return (VkDescriptorPoolCreateInfo v1 v2 v3 v4 v5 v6)
    poke p (VkDescriptorPoolCreateInfo v1 v2 v3 v4 v5 v6) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6

instance Storable VkDescriptorPoolSize where
    sizeOf _    = 8
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        return (VkDescriptorPoolSize v1 v2)
    poke p (VkDescriptorPoolSize v1 v2) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2

instance Storable VkDescriptorSetAllocateInfo where
    sizeOf _    = 40
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        return (VkDescriptorSetAllocateInfo v1 v2 v3 v4 v5)
    poke p (VkDescriptorSetAllocateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5

instance Storable VkDescriptorSetLayoutBinding where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        return (VkDescriptorSetLayoutBinding v1 v2 v3 v4 v5)
    poke p (VkDescriptorSetLayoutBinding v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5

instance Storable VkDescriptorSetLayoutCreateInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        return (VkDescriptorSetLayoutCreateInfo v1 v2 v3 v4 v5)
    poke p (VkDescriptorSetLayoutCreateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5

instance Storable VkDeviceCreateInfo where
    sizeOf _    = 72
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 20
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 32
        v07 <- peekByteOff p 40
        v08 <- peekByteOff p 48
        v09 <- peekByteOff p 56
        v10 <- peekByteOff p 64
        return (VkDeviceCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10)
    poke p (VkDeviceCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 20 v04
        pokeByteOff p 24 v05
        pokeByteOff p 32 v06
        pokeByteOff p 40 v07
        pokeByteOff p 48 v08
        pokeByteOff p 56 v09
        pokeByteOff p 64 v10

instance Storable VkDeviceQueueCreateInfo where
    sizeOf _    = 40
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        return (VkDeviceQueueCreateInfo v1 v2 v3 v4 v5 v6)
    poke p (VkDeviceQueueCreateInfo v1 v2 v3 v4 v5 v6) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6

instance Storable VkExtent2D where
    sizeOf _    = 8
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        return (VkExtent2D v1 v2)
    poke p (VkExtent2D v1 v2) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2

instance Storable VkExtent3D where
    sizeOf _    = 12
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        return (VkExtent3D v1 v2 v3)
    poke p (VkExtent3D v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3

instance Storable VkFenceCreateInfo where
    sizeOf _ = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        return (VkFenceCreateInfo v1 v2 v3)
    poke p (VkFenceCreateInfo v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3

instance Storable VkFramebufferCreateInfo where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 48
        v8 <- peekByteOff p 52
        v9 <- peekByteOff p 56
        return (VkFramebufferCreateInfo v1 v2 v3 v4 v5 v6 v7 v8 v9)
    poke p (VkFramebufferCreateInfo v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 48 v7
        pokeByteOff p 52 v8
        pokeByteOff p 56 v9

instance Storable VkGraphicsPipelineCreateInfo where
    sizeOf _    = 144
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 20
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 32
        v07 <- peekByteOff p 40
        v08 <- peekByteOff p 48
        v09 <- peekByteOff p 56
        v10 <- peekByteOff p 64
        v11 <- peekByteOff p 72
        v12 <- peekByteOff p 80
        v13 <- peekByteOff p 88
        v14 <- peekByteOff p 96
        v15 <- peekByteOff p 104
        v16 <- peekByteOff p 112
        v17 <- peekByteOff p 120
        v18 <- peekByteOff p 128
        v19 <- peekByteOff p 136
        return (VkGraphicsPipelineCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19)
    poke p (VkGraphicsPipelineCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 20 v04
        pokeByteOff p 24 v05
        pokeByteOff p 32 v06
        pokeByteOff p 40 v07
        pokeByteOff p 48 v08
        pokeByteOff p 56 v09
        pokeByteOff p 64 v10
        pokeByteOff p 72 v11
        pokeByteOff p 80 v12
        pokeByteOff p 88 v13
        pokeByteOff p 96 v14
        pokeByteOff p 104 v15
        pokeByteOff p 112 v16
        pokeByteOff p 120 v17
        pokeByteOff p 128 v18
        pokeByteOff p 136 v19

instance Storable VkImageCreateInfo where
    sizeOf _    = 88
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 20
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 28
        v07 <- peekByteOff p 40
        v08 <- peekByteOff p 44
        v09 <- peekByteOff p 48
        v10 <- peekByteOff p 52
        v11 <- peekByteOff p 56
        v12 <- peekByteOff p 60
        v13 <- peekByteOff p 64
        v14 <- peekByteOff p 72
        v15 <- peekByteOff p 80
        return (VkImageCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15)
    poke p (VkImageCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 20 v04
        pokeByteOff p 24 v05
        pokeByteOff p 28 v06
        pokeByteOff p 40 v07
        pokeByteOff p 44 v08
        pokeByteOff p 48 v09
        pokeByteOff p 52 v10
        pokeByteOff p 56 v11
        pokeByteOff p 60 v12
        pokeByteOff p 64 v13
        pokeByteOff p 72 v14
        pokeByteOff p 80 v15

instance Storable VkImageSubresource where
    sizeOf _    = 12
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        return (VkImageSubresource v1 v2 v3)
    poke p (VkImageSubresource v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3

instance Storable VkImageSubresourceRange where
    sizeOf _    = 20
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        return (VkImageSubresourceRange v1 v2 v3 v4 v5)
    poke p (VkImageSubresourceRange v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5

instance Storable VkImageViewCreateInfo where
    sizeOf _    = 80
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 36
        v7 <- peekByteOff p 40
        v8 <- peekByteOff p 56
        return (VkImageViewCreateInfo v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkImageViewCreateInfo v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 36 v6
        pokeByteOff p 40 v7
        pokeByteOff p 56 v8

instance Storable VkInstanceCreateInfo where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 48
        v8 <- peekByteOff p 56
        return (VkInstanceCreateInfo v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkInstanceCreateInfo v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 48 v7
        pokeByteOff p 56 v8

instance Storable VkMemoryAllocateInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        return (VkMemoryAllocateInfo v1 v2 v3 v4)
    poke p (VkMemoryAllocateInfo v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4

instance Storable VkMemoryRequirements where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        return (VkMemoryRequirements v1 v2 v3)
    poke p (VkMemoryRequirements v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3

instance Storable VkOffset2D where
    sizeOf _    = 8
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        return (VkOffset2D v1 v2)
    poke p (VkOffset2D v1 v2) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2

instance Storable VkPhysicalDeviceFeatures where
    sizeOf _    = 220
    alignment _ = 4
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 4
        v03 <- peekByteOff p 8
        v04 <- peekByteOff p 12
        v05 <- peekByteOff p 16
        v06 <- peekByteOff p 20
        v07 <- peekByteOff p 24
        v08 <- peekByteOff p 28
        v09 <- peekByteOff p 32
        v10 <- peekByteOff p 36
        v11 <- peekByteOff p 40
        v12 <- peekByteOff p 44
        v13 <- peekByteOff p 48
        v14 <- peekByteOff p 52
        v15 <- peekByteOff p 56
        v16 <- peekByteOff p 60
        v17 <- peekByteOff p 64
        v18 <- peekByteOff p 68
        v19 <- peekByteOff p 72
        v20 <- peekByteOff p 76
        v21 <- peekByteOff p 80
        v22 <- peekByteOff p 84
        v23 <- peekByteOff p 88
        v24 <- peekByteOff p 92
        v25 <- peekByteOff p 96
        v26 <- peekByteOff p 100
        v27 <- peekByteOff p 104
        v28 <- peekByteOff p 108
        v29 <- peekByteOff p 112
        v30 <- peekByteOff p 116
        v31 <- peekByteOff p 120
        v32 <- peekByteOff p 124
        v33 <- peekByteOff p 128
        v34 <- peekByteOff p 132
        v35 <- peekByteOff p 136
        v36 <- peekByteOff p 140
        v37 <- peekByteOff p 144
        v38 <- peekByteOff p 148
        v39 <- peekByteOff p 152
        v40 <- peekByteOff p 156
        v41 <- peekByteOff p 160
        v42 <- peekByteOff p 164
        v43 <- peekByteOff p 168
        v44 <- peekByteOff p 172
        v45 <- peekByteOff p 176
        v46 <- peekByteOff p 180
        v47 <- peekByteOff p 184
        v48 <- peekByteOff p 188
        v49 <- peekByteOff p 192
        v50 <- peekByteOff p 196
        v51 <- peekByteOff p 200
        v52 <- peekByteOff p 204
        v53 <- peekByteOff p 208
        v54 <- peekByteOff p 212
        v55 <- peekByteOff p 216
        return (VkPhysicalDeviceFeatures v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49 v50 v51 v52 v53 v54 v55)
    poke p (VkPhysicalDeviceFeatures v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49 v50 v51 v52 v53 v54 v55) = do
        pokeByteOff p 0 v01
        pokeByteOff p 4 v02
        pokeByteOff p 8 v03
        pokeByteOff p 12 v04
        pokeByteOff p 16 v05
        pokeByteOff p 20 v06
        pokeByteOff p 24 v07
        pokeByteOff p 28 v08
        pokeByteOff p 32 v09
        pokeByteOff p 36 v10
        pokeByteOff p 40 v11
        pokeByteOff p 44 v12
        pokeByteOff p 48 v13
        pokeByteOff p 52 v14
        pokeByteOff p 56 v15
        pokeByteOff p 60 v16
        pokeByteOff p 64 v17
        pokeByteOff p 68 v18
        pokeByteOff p 72 v19
        pokeByteOff p 76 v20
        pokeByteOff p 80 v21
        pokeByteOff p 84 v22
        pokeByteOff p 88 v23
        pokeByteOff p 92 v24
        pokeByteOff p 96 v25
        pokeByteOff p 100 v26
        pokeByteOff p 104 v27
        pokeByteOff p 108 v28
        pokeByteOff p 112 v29
        pokeByteOff p 116 v30
        pokeByteOff p 120 v31
        pokeByteOff p 124 v32
        pokeByteOff p 128 v33
        pokeByteOff p 132 v34
        pokeByteOff p 136 v35
        pokeByteOff p 140 v36
        pokeByteOff p 144 v37
        pokeByteOff p 148 v38
        pokeByteOff p 152 v39
        pokeByteOff p 156 v40
        pokeByteOff p 160 v41
        pokeByteOff p 164 v42
        pokeByteOff p 168 v43
        pokeByteOff p 172 v44
        pokeByteOff p 176 v45
        pokeByteOff p 180 v46
        pokeByteOff p 184 v47
        pokeByteOff p 188 v48
        pokeByteOff p 192 v49
        pokeByteOff p 196 v50
        pokeByteOff p 200 v51
        pokeByteOff p 204 v52
        pokeByteOff p 208 v53
        pokeByteOff p 212 v54
        pokeByteOff p 216 v55

instance Storable VkPhysicalDeviceFeatures2 where
    sizeOf _    = 240
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        return (VkPhysicalDeviceFeatures2 v01 v02 v03)
    poke p (VkPhysicalDeviceFeatures2 v01 v02 v03) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03

instance Storable VkPhysicalDeviceVulkan11Features where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 20
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 28
        v07 <- peekByteOff p 32
        v08 <- peekByteOff p 36
        v09 <- peekByteOff p 40
        v10 <- peekByteOff p 44
        v11 <- peekByteOff p 48
        v12 <- peekByteOff p 52
        v13 <- peekByteOff p 56
        v14 <- peekByteOff p 60
        return (VkPhysicalDeviceVulkan11Features v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14)
    poke p (VkPhysicalDeviceVulkan11Features v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 20 v04
        pokeByteOff p 24 v05
        pokeByteOff p 28 v06
        pokeByteOff p 32 v07
        pokeByteOff p 36 v08
        pokeByteOff p 40 v09
        pokeByteOff p 44 v10
        pokeByteOff p 48 v11
        pokeByteOff p 52 v12
        pokeByteOff p 56 v13
        pokeByteOff p 60 v14

instance Storable VkPhysicalDeviceVulkan12Features where
    sizeOf _    = 200
    alignment _ = 4
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 4
        v03 <- peekByteOff p 12
        v04 <- peekByteOff p 16
        v05 <- peekByteOff p 20
        v06 <- peekByteOff p 24
        v07 <- peekByteOff p 28
        v08 <- peekByteOff p 32
        v09 <- peekByteOff p 36
        v10 <- peekByteOff p 40
        v11 <- peekByteOff p 44
        v12 <- peekByteOff p 48
        v13 <- peekByteOff p 52
        v14 <- peekByteOff p 56
        v15 <- peekByteOff p 60
        v16 <- peekByteOff p 64
        v17 <- peekByteOff p 68
        v18 <- peekByteOff p 72
        v19 <- peekByteOff p 76
        v20 <- peekByteOff p 80
        v21 <- peekByteOff p 84
        v22 <- peekByteOff p 88
        v23 <- peekByteOff p 92
        v24 <- peekByteOff p 96
        v25 <- peekByteOff p 100
        v26 <- peekByteOff p 104
        v27 <- peekByteOff p 108
        v28 <- peekByteOff p 112
        v29 <- peekByteOff p 116
        v30 <- peekByteOff p 120
        v31 <- peekByteOff p 124
        v32 <- peekByteOff p 128
        v33 <- peekByteOff p 132
        v34 <- peekByteOff p 136
        v35 <- peekByteOff p 140
        v36 <- peekByteOff p 144
        v37 <- peekByteOff p 148
        v38 <- peekByteOff p 152
        v39 <- peekByteOff p 156
        v40 <- peekByteOff p 160
        v41 <- peekByteOff p 164
        v42 <- peekByteOff p 168
        v43 <- peekByteOff p 172
        v44 <- peekByteOff p 176
        v45 <- peekByteOff p 180
        v46 <- peekByteOff p 184
        v47 <- peekByteOff p 188
        v48 <- peekByteOff p 192
        v49 <- peekByteOff p 196
        return (VkPhysicalDeviceVulkan12Features v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49)
    poke p (VkPhysicalDeviceVulkan12Features v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49) = do
        pokeByteOff p 0 v01
        pokeByteOff p 4 v02
        pokeByteOff p 12 v03
        pokeByteOff p 16 v04
        pokeByteOff p 20 v05
        pokeByteOff p 24 v06
        pokeByteOff p 28 v07
        pokeByteOff p 32 v08
        pokeByteOff p 36 v09
        pokeByteOff p 40 v10
        pokeByteOff p 44 v11
        pokeByteOff p 48 v12
        pokeByteOff p 52 v13
        pokeByteOff p 56 v14
        pokeByteOff p 60 v15
        pokeByteOff p 64 v16
        pokeByteOff p 68 v17
        pokeByteOff p 72 v18
        pokeByteOff p 76 v19
        pokeByteOff p 80 v20
        pokeByteOff p 84 v21
        pokeByteOff p 88 v22
        pokeByteOff p 92 v23
        pokeByteOff p 96 v24
        pokeByteOff p 100 v25
        pokeByteOff p 104 v26
        pokeByteOff p 108 v27
        pokeByteOff p 112 v28
        pokeByteOff p 116 v29
        pokeByteOff p 120 v30
        pokeByteOff p 124 v31
        pokeByteOff p 128 v32
        pokeByteOff p 132 v33
        pokeByteOff p 136 v34
        pokeByteOff p 140 v35
        pokeByteOff p 144 v36
        pokeByteOff p 148 v37
        pokeByteOff p 152 v38
        pokeByteOff p 156 v39
        pokeByteOff p 160 v40
        pokeByteOff p 164 v41
        pokeByteOff p 168 v42
        pokeByteOff p 172 v43
        pokeByteOff p 176 v44
        pokeByteOff p 180 v45
        pokeByteOff p 184 v46
        pokeByteOff p 188 v47
        pokeByteOff p 192 v48
        pokeByteOff p 196 v49

instance Storable VkPipelineCacheCreateInfo where
    sizeOf _    = 40
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        return (VkPipelineCacheCreateInfo v1 v2 v3 v4 v5)
    poke p (VkPipelineCacheCreateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5

instance Storable VkPipelineColorBlendAttachmentState where
    sizeOf _    = 32
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        v6 <- peekByteOff p 20
        v7 <- peekByteOff p 24
        v8 <- peekByteOff p 28
        return (VkPipelineColorBlendAttachmentState v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkPipelineColorBlendAttachmentState v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5
        pokeByteOff p 20 v6
        pokeByteOff p 24 v7
        pokeByteOff p 28 v8

instance Storable VkPipelineColorBlendStateCreateInfo where
    sizeOf _    = 56
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 28
        v7 <- peekByteOff p 32
        v8 <- peekByteOff p 40
        return (VkPipelineColorBlendStateCreateInfo v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkPipelineColorBlendStateCreateInfo v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 28 v6
        pokeByteOff p 32 v7
        pokeByteOff p 40 v8

instance Storable VkPipelineDepthStencilStateCreateInfo where
    sizeOf _    = 104
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 20
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 28
        v07 <- peekByteOff p 32
        v08 <- peekByteOff p 36
        v09 <- peekByteOff p 40
        v10 <- peekByteOff p 68
        v11 <- peekByteOff p 96
        v12 <- peekByteOff p 100
        return (VkPipelineDepthStencilStateCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12)
    poke p (VkPipelineDepthStencilStateCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 20 v04
        pokeByteOff p 24 v05
        pokeByteOff p 28 v06
        pokeByteOff p 32 v07
        pokeByteOff p 36 v08
        pokeByteOff p 40 v09
        pokeByteOff p 68 v10
        pokeByteOff p 96 v11
        pokeByteOff p 100 v12

instance Storable VkPipelineDynamicStateCreateInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        return (VkPipelineDynamicStateCreateInfo v1 v2 v3 v4 v5)
    poke p (VkPipelineDynamicStateCreateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5

instance Storable VkPipelineInputAssemblyStateCreateInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        return (VkPipelineInputAssemblyStateCreateInfo v1 v2 v3 v4 v5)
    poke p (VkPipelineInputAssemblyStateCreateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5

instance Storable VkPipelineLayoutCreateInfo where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        v7 <- peekByteOff p 40
        return (VkPipelineLayoutCreateInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkPipelineLayoutCreateInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6
        pokeByteOff p 40 v7

instance Storable VkPipelineMultisampleStateCreateInfo where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 28
        v7 <- peekByteOff p 32
        v8 <- peekByteOff p 40
        v9 <- peekByteOff p 44
        return (VkPipelineMultisampleStateCreateInfo v1 v2 v3 v4 v5 v6 v7 v8 v9)
    poke p (VkPipelineMultisampleStateCreateInfo v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 28 v6
        pokeByteOff p 32 v7
        pokeByteOff p 40 v8
        pokeByteOff p 44 v9

instance Storable VkPipelineRasterizationStateCreateInfo where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 20
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 28
        v07 <- peekByteOff p 32
        v08 <- peekByteOff p 36
        v09 <- peekByteOff p 40
        v10 <- peekByteOff p 44
        v11 <- peekByteOff p 48
        v12 <- peekByteOff p 52
        v13 <- peekByteOff p 56
        return (VkPipelineRasterizationStateCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13)
    poke p (VkPipelineRasterizationStateCreateInfo v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 20 v04
        pokeByteOff p 24 v05
        pokeByteOff p 28 v06
        pokeByteOff p 32 v07
        pokeByteOff p 36 v08
        pokeByteOff p 40 v09
        pokeByteOff p 44 v10
        pokeByteOff p 48 v11
        pokeByteOff p 52 v12
        pokeByteOff p 56 v13

instance Storable VkPipelineShaderStageCreateInfo where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        v7 <- peekByteOff p 40
        return (VkPipelineShaderStageCreateInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkPipelineShaderStageCreateInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6
        pokeByteOff p 40 v7

instance Storable VkPipelineTessellationStateCreateInfo where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        return (VkPipelineTessellationStateCreateInfo v1 v2 v3 v4)
    poke p (VkPipelineTessellationStateCreateInfo v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4

instance Storable VkPipelineVertexInputStateCreateInfo where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        v7 <- peekByteOff p 40
        return (VkPipelineVertexInputStateCreateInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkPipelineVertexInputStateCreateInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6
        pokeByteOff p 40 v7

instance Storable VkPipelineViewportStateCreateInfo where
    sizeOf _    = 48
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        v7 <- peekByteOff p 40
        return (VkPipelineViewportStateCreateInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkPipelineViewportStateCreateInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6
        pokeByteOff p 40 v7

instance Storable VkPresentInfoKHR where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 48
        v8 <- peekByteOff p 56
        return (VkPresentInfoKHR v1 v2 v3 v4 v5 v6 v7 v8)
    poke p (VkPresentInfoKHR v1 v2 v3 v4 v5 v6 v7 v8) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 48 v7
        pokeByteOff p 56 v8

instance Storable VkPushConstantRange where
    sizeOf _    = 12
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        return (VkPushConstantRange v1 v2 v3)
    poke p (VkPushConstantRange v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3

instance Storable VkRect2D where
    sizeOf _    = 16
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        return (VkRect2D v1 v2)
    poke p (VkRect2D v1 v2) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2

instance Storable VkRenderPassBeginInfo where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 48
        v7 <- peekByteOff p 56
        return (VkRenderPassBeginInfo v1 v2 v3 v4 v5 v6 v7)
    poke p (VkRenderPassBeginInfo v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 48 v6
        pokeByteOff p 56 v7

instance Storable VkRenderPassCreateInfo where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 20
        v5 <- peekByteOff p 24
        v6 <- peekByteOff p 32
        v7 <- peekByteOff p 40
        v8 <- peekByteOff p 48
        v9 <- peekByteOff p 56
        return (VkRenderPassCreateInfo v1 v2 v3 v4 v5 v6 v7 v8 v9)
    poke p (VkRenderPassCreateInfo v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 20 v4
        pokeByteOff p 24 v5
        pokeByteOff p 32 v6
        pokeByteOff p 40 v7
        pokeByteOff p 48 v8
        pokeByteOff p 56 v9

instance Storable VkSemaphoreCreateInfo where
    sizeOf _    = 24
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        return (VkSemaphoreCreateInfo v1 v2 v3)
    poke p (VkSemaphoreCreateInfo v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3

instance Storable VkSemaphoreTypeCreateInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        return (VkSemaphoreTypeCreateInfo v1 v2 v3 v4)
    poke p (VkSemaphoreTypeCreateInfo v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4

instance Storable VkShaderModuleCreateInfo where
    sizeOf _    = 40
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        return (VkShaderModuleCreateInfo v1 v2 v3 v4 v5)
    poke p (VkShaderModuleCreateInfo v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5

instance Storable VkSpecializationInfo where
    sizeOf _    = 32
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        return (VkSpecializationInfo v1 v2 v3 v4)
    poke p (VkSpecializationInfo v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4

instance Storable VkSpecializationMapEntry where
    sizeOf _    = 16
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        return (VkSpecializationMapEntry v1 v2 v3)
    poke p (VkSpecializationMapEntry v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3

instance Storable VkStencilOpState where
    sizeOf _    = 28
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        v6 <- peekByteOff p 20
        v7 <- peekByteOff p 24
        return (VkStencilOpState v1 v2 v3 v4 v5 v6 v7)
    poke p (VkStencilOpState v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5
        pokeByteOff p 20 v6
        pokeByteOff p 24 v7

instance Storable VkSubmitInfo where
    sizeOf _    = 72
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        v6 <- peekByteOff p 40
        v7 <- peekByteOff p 48
        v8 <- peekByteOff p 56
        v9 <- peekByteOff p 64
        return (VkSubmitInfo v1 v2 v3 v4 v5 v6 v7 v8 v9)
    poke p (VkSubmitInfo v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5
        pokeByteOff p 40 v6
        pokeByteOff p 48 v7
        pokeByteOff p 56 v8
        pokeByteOff p 64 v9

instance Storable VkSubpassDependency where
    sizeOf _    = 28
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        v6 <- peekByteOff p 20
        v7 <- peekByteOff p 24
        return (VkSubpassDependency v1 v2 v3 v4 v5 v6 v7)
    poke p (VkSubpassDependency v1 v2 v3 v4 v5 v6 v7) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5
        pokeByteOff p 20 v6
        pokeByteOff p 24 v7

instance Storable VkSubpassDescription where
    sizeOf _    = 72
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 4
        v03 <- peekByteOff p 8
        v04 <- peekByteOff p 16
        v05 <- peekByteOff p 24
        v06 <- peekByteOff p 32
        v07 <- peekByteOff p 40
        v08 <- peekByteOff p 48
        v09 <- peekByteOff p 56
        v10 <- peekByteOff p 64
        return (VkSubpassDescription v01 v02 v03 v04 v05 v06 v07 v08 v09 v10)
    poke p (VkSubpassDescription v01 v02 v03 v04 v05 v06 v07 v08 v09 v10) = do
        pokeByteOff p 0 v01
        pokeByteOff p 4 v02
        pokeByteOff p 8 v03
        pokeByteOff p 16 v04
        pokeByteOff p 24 v05
        pokeByteOff p 32 v06
        pokeByteOff p 40 v07
        pokeByteOff p 48 v08
        pokeByteOff p 56 v09
        pokeByteOff p 64 v10

instance Storable VkSubresourceLayout where
    sizeOf _    = 40
    alignment _ = 8
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 8
        v3 <- peekByteOff p 16
        v4 <- peekByteOff p 24
        v5 <- peekByteOff p 32
        return (VkSubresourceLayout v1 v2 v3 v4 v5)
    poke p (VkSubresourceLayout v1 v2 v3 v4 v5) = do
        pokeByteOff p 0 v1
        pokeByteOff p 8 v2
        pokeByteOff p 16 v3
        pokeByteOff p 24 v4
        pokeByteOff p 32 v5

instance Storable VkSwapchainCreateInfoKHR where
    sizeOf _    = 104
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 24
        v05 <- peekByteOff p 32
        v06 <- peekByteOff p 36
        v07 <- peekByteOff p 40
        v08 <- peekByteOff p 44
        v09 <- peekByteOff p 52
        v10 <- peekByteOff p 56
        v11 <- peekByteOff p 60
        v12 <- peekByteOff p 64
        v13 <- peekByteOff p 72
        v14 <- peekByteOff p 80
        v15 <- peekByteOff p 84
        v16 <- peekByteOff p 88
        v17 <- peekByteOff p 92
        v18 <- peekByteOff p 96
        return (VkSwapchainCreateInfoKHR v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18)
    poke p (VkSwapchainCreateInfoKHR v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 24 v04
        pokeByteOff p 32 v05
        pokeByteOff p 36 v06
        pokeByteOff p 40 v07
        pokeByteOff p 44 v08
        pokeByteOff p 52 v09
        pokeByteOff p 56 v10
        pokeByteOff p 60 v11
        pokeByteOff p 64 v12
        pokeByteOff p 72 v13
        pokeByteOff p 80 v14
        pokeByteOff p 84 v15
        pokeByteOff p 88 v16
        pokeByteOff p 92 v17
        pokeByteOff p 96 v18

instance Storable VkVertexInputAttributeDescription where
    sizeOf _    = 16
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        return (VkVertexInputAttributeDescription v1 v2 v3 v4)
    poke p (VkVertexInputAttributeDescription v1 v2 v3 v4) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4

instance Storable VkVertexInputBindingDescription where
    sizeOf _    = 12
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        return (VkVertexInputBindingDescription v1 v2 v3)
    poke p (VkVertexInputBindingDescription v1 v2 v3) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3

instance Storable VkViewport where
    sizeOf _    = 24
    alignment _ = 4
    peek p = do
        v1 <- peekByteOff p 0
        v2 <- peekByteOff p 4
        v3 <- peekByteOff p 8
        v4 <- peekByteOff p 12
        v5 <- peekByteOff p 16
        v6 <- peekByteOff p 20
        return (VkViewport v1 v2 v3 v4 v5 v6)
    poke p (VkViewport v1 v2 v3 v4 v5 v6) = do
        pokeByteOff p 0 v1
        pokeByteOff p 4 v2
        pokeByteOff p 8 v3
        pokeByteOff p 12 v4
        pokeByteOff p 16 v5
        pokeByteOff p 20 v6

instance Storable VkWriteDescriptorSet where
    sizeOf _    = 64
    alignment _ = 8
    peek p = do
        v01 <- peekByteOff p 0
        v02 <- peekByteOff p 8
        v03 <- peekByteOff p 16
        v04 <- peekByteOff p 24
        v05 <- peekByteOff p 28
        v06 <- peekByteOff p 32
        v07 <- peekByteOff p 36
        v08 <- peekByteOff p 40
        v09 <- peekByteOff p 48
        v10 <- peekByteOff p 56
        return (VkWriteDescriptorSet v01 v02 v03 v04 v05 v06 v07 v08 v09 v10)
    poke p (VkWriteDescriptorSet v01 v02 v03 v04 v05 v06 v07 v08 v09 v10) = do
        pokeByteOff p 0 v01
        pokeByteOff p 8 v02
        pokeByteOff p 16 v03
        pokeByteOff p 24 v04
        pokeByteOff p 28 v05
        pokeByteOff p 32 v06
        pokeByteOff p 36 v07
        pokeByteOff p 40 v08
        pokeByteOff p 48 v09
        pokeByteOff p 56 v10
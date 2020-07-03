{-# LANGUAGE DuplicateRecordFields, Safe #-}

module Graphics.Vulkan.Data where


import Data.Void (Void)
import Data.Word (Word8, Word32)

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

data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineCreateFlags,
    stage :: VkPipelineShaderStageCreateInfo,
    layout :: VkPipelineLayout,
    basePipelineHandle :: VkPipeline,
    basePipelineIndex :: Int32
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

data VkExtent3D = VkExtent3D{
    width :: Word32,
    height :: Word32,
    depth :: Word32
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
}

data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineCacheCreateFlags,
    initialDataSize :: CSize,
    pInitialData :: Ptr Void
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

data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo{
    sType :: VkStructureType,
    next :: Ptr Void,
    flags :: VkPipelineShaderStageCreateFlags,
    stage :: VkShaderStageFlagBits,
    vkModule :: VkShaderModule,
    pName :: CString,
    pSpecializationInfo :: Ptr VkSpecializationInfo
}

data VkPushConstantRange = VkPushConstantRange{
    stageFlags :: VkShaderStageFlags,
    offset :: Word32,
    size :: Word32
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

data VkSubresourceLayout = VkSubresourceLayout{
    offset :: VkDeviceSize,
    size :: VkDeviceSize,
    rowPitch :: VkDeviceSize,
    arrayPitch :: VkDeviceSize,
    depthPitch :: VkDeviceSize
}

-- Storable instances
instance Storable VkAllocationCallbacks where
    sizeOf _ = 48
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
    sizeOf _ = 48
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

instance Storable VkBufferCreateInfo where
    sizeOf _ = 56
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
    sizeOf _ = 16
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkClearColorValue v)
    poke p (VkClearColorValue v) = pokeByteOff p 0 v

instance Storable VkComputePipelineCreateInfo where
    sizeOf _ = 96
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

instance Storable VkDescriptorSetLayoutBinding where
    sizeOf _ = 24
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
    sizeOf _ = 32
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
    sizeOf _ = 72
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
    sizeOf _ = 40
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

instance Storable VkExtent3D where
    sizeOf _ = 12
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

instance Storable VkImageCreateInfo where
    sizeOf _ = 88
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
    sizeOf _ = 12
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

instance Storable VkInstanceCreateInfo where
    sizeOf _ = 64
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
    sizeOf _ = 32
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
    sizeOf _ = 24
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

instance Storable VkPhysicalDeviceFeatures where
    sizeOf _ = 220
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

instance Storable VkPipelineCacheCreateInfo where
    sizeOf _ = 40
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

instance Storable VkPipelineLayoutCreateInfo where
    sizeOf _ = 48
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

instance Storable VkPipelineShaderStageCreateInfo where
    sizeOf _ = 48
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

instance Storable VkPushConstantRange where
    sizeOf _ = 12
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

instance Storable VkShaderModuleCreateInfo where
    sizeOf _ = 40
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
    sizeOf _ = 32
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
    sizeOf _ = 16
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

instance Storable VkSubresourceLayout where
    sizeOf _ = 40
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
{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Enumerations where


import Data.Word (Word32)

import Foreign


-- Vulkan newtypes
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits { unVkBufferUsageFlagBits :: Word32 }
    deriving (Eq)
newtype VkInternalAllocationType = VkInternalAllocationType { unVkInternalAllocationType :: Word32 }
    deriving (Eq)
newtype VkResult = VkResult { unVkResult :: Int32 }
    deriving (Eq)
newtype VkSharingMode = VkSharingMode { unVkSharingMode :: Word32 }
    deriving (Eq)
newtype VkStructureType = VkStructureType { unVkStructureType :: Int32 }
    deriving (Eq)
newtype VkSystemAllocationScope = VkSystemAllocationScope { unVkSystemAllocationScope :: Word32 }
    deriving (Eq)


-- Vulkan enumerations
-- VkBufferUsageFlagBits
bufferUsageTransferSRCBit            :: VkBufferUsageFlagBits
bufferUsageTransferSRCBit            = VkBufferUsageFlagBits 1
bufferUsageTransferDSTBit            :: VkBufferUsageFlagBits
bufferUsageTransferDSTBit            = VkBufferUsageFlagBits 2
bufferUsageUniformTexelBufferBit     :: VkBufferUsageFlagBits
bufferUsageUniformTexelBufferBit     = VkBufferUsageFlagBits 4
bufferUsageStorageTexelBufferBit     :: VkBufferUsageFlagBits
bufferUsageStorageTexelBufferBit     = VkBufferUsageFlagBits 8
bufferUsageUniformBufferBit          :: VkBufferUsageFlagBits
bufferUsageUniformBufferBit          = VkBufferUsageFlagBits 16
bufferUsageStorageBufferBit          :: VkBufferUsageFlagBits
bufferUsageStorageBufferBit          = VkBufferUsageFlagBits 32
bufferUsageIndexBufferBit            :: VkBufferUsageFlagBits
bufferUsageIndexBufferBit            = VkBufferUsageFlagBits 64
bufferUsageVertexBufferBit           :: VkBufferUsageFlagBits
bufferUsageVertexBufferBit           = VkBufferUsageFlagBits 128
bufferUsageIndirectBufferBit         :: VkBufferUsageFlagBits
bufferUsageIndirectBufferBit         = VkBufferUsageFlagBits 256

-- VkInternalAllocationType
internalAllocationTypeExecutable     :: VkInternalAllocationType
internalAllocationTypeExecutable     = VkInternalAllocationType 0

-- VkResult
success                      :: VkResult
success                      = VkResult 0
notReady                     :: VkResult
notReady                     = VkResult 1
timeout                      :: VkResult
timeout                      = VkResult 2
eventSet                     :: VkResult
eventSet                     = VkResult 3
eventReset                   :: VkResult
eventReset                   = VkResult 4
incomplete                   :: VkResult
incomplete                   = VkResult 5
errorOutOfHostMemory         :: VkResult
errorOutOfHostMemory         = VkResult (-1)
errorOutOfDeviceMemory       :: VkResult
errorOutOfDeviceMemory       = VkResult (-2)
errorInitializationFailed    :: VkResult
errorInitializationFailed    = VkResult (-3)
errorDeviceLost              :: VkResult
errorDeviceLost              = VkResult (-4)
errorMemoryFailed            :: VkResult
errorMemoryFailed            = VkResult (-5)
errorLayerNotPresent         :: VkResult
errorLayerNotPresent         = VkResult (-6)
errorExtensionNotPresent     :: VkResult
errorExtensionNotPresent     = VkResult (-7)
errorFeatureNotPresent       :: VkResult
errorFeatureNotPresent       = VkResult (-8)
errorIncompatibleDriver      :: VkResult
errorIncompatibleDriver      = VkResult (-9)
errorTooManyObjects          :: VkResult
errorTooManyObjects          = VkResult (-10)
errorFormatNotSupported      :: VkResult
errorFormatNotSupported      = VkResult (-11)
errorFragmentedPool          :: VkResult
errorFragmentedPool          = VkResult (-12)
errorSurfaceLostKHR          :: VkResult
errorSurfaceLostKHR          = VkResult (-1000000000)
errorNativeWindowInUseKHR    :: VkResult
errorNativeWindowInUseKHR    = VkResult (-1000000001)
suboptimalKHR                :: VkResult
suboptimalKHR                = VkResult 1000001003
errorOutOfDateKHR            :: VkResult
errorOutOfDateKHR            = VkResult (-1000001004)
errorIncompatibleDisplayKHR  :: VkResult
errorIncompatibleDisplayKHR  = VkResult (-1000003001)
errorValidationFailedEXT     :: VkResult
errorValidationFailedEXT     = VkResult (-1000011001)
errorInvalidShaderNV         :: VkResult
errorInvalidShaderNV         = VkResult (-1000012000)

-- VkSharingMode
sharingModeExclusive     :: VkSharingMode
sharingModeExclusive     = VkSharingMode 0
sharingModeConcurrent    :: VkSharingMode
sharingModeConcurrent    = VkSharingMode 1

-- VkStructureType
structureTypeApplicationInfo                                  :: VkStructureType
structureTypeApplicationInfo                                  = VkStructureType 0
structureTypeInstanceCreateInfo                               :: VkStructureType
structureTypeInstanceCreateInfo                               = VkStructureType 1
structureTypeDeviceQueueCreateInfo                            :: VkStructureType
structureTypeDeviceQueueCreateInfo                            = VkStructureType 2
structureTypeDeviceCreateInfo                                 :: VkStructureType
structureTypeDeviceCreateInfo                                 = VkStructureType 3
structureTypeSubmitInfo                                       :: VkStructureType
structureTypeSubmitInfo                                       = VkStructureType 4
structureTypeMemoryAllocateInfo                               :: VkStructureType
structureTypeMemoryAllocateInfo                               = VkStructureType 5
structureTypeMappedMemoryRange                                :: VkStructureType
structureTypeMappedMemoryRange                                = VkStructureType 6
structureTypeBindSparseInfo                                   :: VkStructureType
structureTypeBindSparseInfo                                   = VkStructureType 7
structureTypeFenceCreateInfo                                  :: VkStructureType
structureTypeFenceCreateInfo                                  = VkStructureType 8
structureTypeSemaphreCreateInfo                               :: VkStructureType
structureTypeSemaphreCreateInfo                               = VkStructureType 9
structureTypeEventCreateInfo                                  :: VkStructureType
structureTypeEventCreateInfo                                  = VkStructureType 10
structureTypeQueryPoolCreateInfo                              :: VkStructureType
structureTypeQueryPoolCreateInfo                              = VkStructureType 11
structureTypeBufferCreateInfo                                 :: VkStructureType
structureTypeBufferCreateInfo                                 = VkStructureType 12
structureTypeBufferViewCreateInfo                             :: VkStructureType
structureTypeBufferViewCreateInfo                             = VkStructureType 13
structureTypeImageCreateInfo                                  :: VkStructureType
structureTypeImageCreateInfo                                  = VkStructureType 14
structureTypeImageViewCreateInfo                              :: VkStructureType
structureTypeImageViewCreateInfo                              = VkStructureType 15
structureTypeShaderModuleCreateInfo                           :: VkStructureType
structureTypeShaderModuleCreateInfo                           = VkStructureType 16
structureTypePipelineCacheCreateInfo                          :: VkStructureType
structureTypePipelineCacheCreateInfo                          = VkStructureType 17
structureTypePipelineShaderStageCreateInfo                    :: VkStructureType
structureTypePipelineShaderStageCreateInfo                    = VkStructureType 18
structureTypePipelineVertexInputStateCreateInfo               :: VkStructureType
structureTypePipelineVertexInputStateCreateInfo               = VkStructureType 19
structureTypePipelineInputAssembyStateCreateInfo              :: VkStructureType
structureTypePipelineInputAssembyStateCreateInfo              = VkStructureType 20
structureTypePipelineTessellationStateCreateInfo              :: VkStructureType
structureTypePipelineTessellationStateCreateInfo              = VkStructureType 21
structureTypePipelineViewportStateCreateInfo                  :: VkStructureType
structureTypePipelineViewportStateCreateInfo                  = VkStructureType 22
structureTypePipelineRaterizationStateCreateInfo              :: VkStructureType
structureTypePipelineRaterizationStateCreateInfo              = VkStructureType 23
structureTypePipelineMultisampleStateCreateInfo               :: VkStructureType
structureTypePipelineMultisampleStateCreateInfo               = VkStructureType 24
structureTypePipelineDepthStencilStateCreateInfo              :: VkStructureType
structureTypePipelineDepthStencilStateCreateInfo              = VkStructureType 25
structureTypePipelineColorBlendStateCreateInfo                :: VkStructureType
structureTypePipelineColorBlendStateCreateInfo                = VkStructureType 26
structureTypePipelineDynamicStateCreateInfo                   :: VkStructureType
structureTypePipelineDynamicStateCreateInfo                   = VkStructureType 27
structureTypeGraphicsPipelineCreateInfo                       :: VkStructureType
structureTypeGraphicsPipelineCreateInfo                       = VkStructureType 28
structureTypeComputePipelineCreateInfo                        :: VkStructureType
structureTypeComputePipelineCreateInfo                        = VkStructureType 29
structureTypePipelineLayoutCreateInfo                         :: VkStructureType
structureTypePipelineLayoutCreateInfo                         = VkStructureType 30
structureTypeSamplerCreateInfo                                :: VkStructureType
structureTypeSamplerCreateInfo                                = VkStructureType 31
structureTypeDescriptorSetLayoutCreateInfo                    :: VkStructureType
structureTypeDescriptorSetLayoutCreateInfo                    = VkStructureType 32
structureTypeDescriptorPoolCreateInfo                         :: VkStructureType
structureTypeDescriptorPoolCreateInfo                         = VkStructureType 33
structureTypeDescriptorSetAllocateInfo                        :: VkStructureType
structureTypeDescriptorSetAllocateInfo                        = VkStructureType 34
structureTypeWriteDescriptorSet                               :: VkStructureType
structureTypeWriteDescriptorSet                               = VkStructureType 35
structureTypeCopyDescriptorSet                                :: VkStructureType
structureTypeCopyDescriptorSet                                = VkStructureType 36
structureTypeFramebufferCreateInfo                            :: VkStructureType
structureTypeFramebufferCreateInfo                            = VkStructureType 37
structureTypeRenderPassCreateInfo                             :: VkStructureType
structureTypeRenderPassCreateInfo                             = VkStructureType 38
structureTypeCommandPoolCreateInfo                            :: VkStructureType
structureTypeCommandPoolCreateInfo                            = VkStructureType 39
structureTypeCommandBufferAllocateInfo                        :: VkStructureType
structureTypeCommandBufferAllocateInfo                        = VkStructureType 40
structureTypeCommandBufferInheritanceInfo                     :: VkStructureType
structureTypeCommandBufferInheritanceInfo                     = VkStructureType 41
structureTypeCommandBufferBeginInfo                           :: VkStructureType
structureTypeCommandBufferBeginInfo                           = VkStructureType 42
structureTypeRenderPassBeginInfo                              :: VkStructureType
structureTypeRenderPassBeginInfo                              = VkStructureType 43
structureTypeBufferMemoryBarrier                              :: VkStructureType
structureTypeBufferMemoryBarrier                              = VkStructureType 44
structureTypeImageMemoryBarrier                               :: VkStructureType
structureTypeImageMemoryBarrier                               = VkStructureType 45
structureTypeMemoryBarrier                                    :: VkStructureType
structureTypeMemoryBarrier                                    = VkStructureType 46
structureTypeLoaderInstanceCreateInfo                         :: VkStructureType
structureTypeLoaderInstanceCreateInfo                         = VkStructureType 47
structureTypeLoaderDeviceCreateInfo                           :: VkStructureType
structureTypeLoaderDeviceCreateInfo                           = VkStructureType 48
structureTypeSwapchainCreateInfoKHR                           :: VkStructureType
structureTypeSwapchainCreateInfoKHR                           = VkStructureType 1000001000
structureTypePresentInfoKHR                                   :: VkStructureType
structureTypePresentInfoKHR                                   = VkStructureType 1000001001
structureTypeDisplayModeCreateInfoKHR                         :: VkStructureType
structureTypeDisplayModeCreateInfoKHR                         = VkStructureType 1000002000
structureTypeDisplaySurfaceCreateInfoKHR                      :: VkStructureType
structureTypeDisplaySurfaceCreateInfoKHR                      = VkStructureType 1000002001
structureTypeDisplayPresentInfoKHR                            :: VkStructureType
structureTypeDisplayPresentInfoKHR                            = VkStructureType 1000003000
structureTypeDebugReportCallbackCreateInfoEXT                 :: VkStructureType
structureTypeDebugReportCallbackCreateInfoEXT                 = VkStructureType 1000011000
structureTypePipelineRasterizationStateRasterizationOrderAMD  :: VkStructureType
structureTypePipelineRasterizationStateRasterizationOrderAMD  = VkStructureType 1000018000
structureTypeDebugMarkerObjectNameInfoEXT                     :: VkStructureType
structureTypeDebugMarkerObjectNameInfoEXT                     = VkStructureType 1000022000
structureTypeDebugMarkerObjectTagInfoEXT                      :: VkStructureType
structureTypeDebugMarkerObjectTagInfoEXT                      = VkStructureType 1000022001
structureTypeDebugMarkerMarkerInfoEXT                         :: VkStructureType
structureTypeDebugMarkerMarkerInfoEXT                         = VkStructureType 1000022002
structureTypeExternalMemoryImageCreateInfoNV                  :: VkStructureType
structureTypeExternalMemoryImageCreateInfoNV                  = VkStructureType 1000056000
structureTypeExportMemoryAllocateInfoNV                       :: VkStructureType
structureTypeExportMemoryAllocateInfoNV                       = VkStructureType 1000056001
structureTypeValidationFlagsEXT                               :: VkStructureType
structureTypeValidationFlagsEXT                               = VkStructureType 1000061000

-- VkSystemAllocationScope
systemAllocationScopeCommand     :: VkSystemAllocationScope
systemAllocationScopeCommand     = VkSystemAllocationScope 0
systemAllocationScopeObject      :: VkSystemAllocationScope
systemAllocationScopeObject      = VkSystemAllocationScope 1
systemAllocationScopeCache       :: VkSystemAllocationScope
systemAllocationScopeCache       = VkSystemAllocationScope 2
systemAllocationScopeDevice      :: VkSystemAllocationScope
systemAllocationScopeDevice      = VkSystemAllocationScope 3
systemAllocationScopeInstance    :: VkSystemAllocationScope
systemAllocationScopeInstance    = VkSystemAllocationScope 4


instance Storable VkBufferUsageFlagBits where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferUsageFlagBits v)
    poke p (VkBufferUsageFlagBits v) = pokeByteOff p 0 v

instance Storable VkInternalAllocationType where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkInternalAllocationType v)
    poke p (VkInternalAllocationType v) = pokeByteOff p 0 v

instance Storable VkResult where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkResult v)
    poke p (VkResult v) = pokeByteOff p 0 v

instance Storable VkSharingMode where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSharingMode v)
    poke p (VkSharingMode v) = pokeByteOff p 0 v

instance Storable VkStructureType where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkStructureType v)
    poke p (VkStructureType v) = pokeByteOff p 0 v

instance Storable VkSystemAllocationScope where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSystemAllocationScope v)
    poke p (VkSystemAllocationScope v) = pokeByteOff p 0 v

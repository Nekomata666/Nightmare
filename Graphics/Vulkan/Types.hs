{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Types where


import Data.Void (Void)
import Data.Word (Word32, Word64)

import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable

import Graphics.Vulkan.Enumerations (VkInternalAllocationType, VkSystemAllocationScope)


-- Vulkan Type aliases.
type Next       = Ptr Void
type VkFlags    = Word32
type VkHandle   = Word64

-- Vulkan pre-defines
nullHandle :: VkHandle
nullHandle = 0

-- Vulkan Types
-- 0 = False, 1 = True
newtype VkBool  = VkBool { unVkBool :: Word32 }
     deriving (Show)
newtype VkSampleMask = VkSampleMask { unVkSampleMask :: Word32 }

-- Vulkan Flags
newtype VkAccessFlags = VkAccessFlags { unVkAccessFlags :: VkFlags }
newtype VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlags { unVkAttachmentDescriptionFlags :: VkFlags }
newtype VkBufferCreateFlags = VkBufferCreateFlags { unVkBufferCreateFlags :: VkFlags }
newtype VkBufferUsageFlags = VkBufferUsageFlags { unVkBufferUsageFlags :: VkFlags }
newtype VkColorComponentFlags = VkColorComponentFlags { unVkColorComponentFlags :: VkFlags }
newtype VkCommandBufferUsageFlags = VkCommandBufferUsageFlags { unVkCommandBufferUsageFlags :: VkFlags }
newtype VkCommandPoolCreateFlags = VkCommandPoolCreateFlags { unVkCommandPoolCreateFlags :: VkFlags }
newtype VkCullModeFlags = VkCullModeFlags { unVkCullModeFlags :: VkFlags }
newtype VkDependencyFlags = VkDependencyFlags { unVkDependencyFlags :: VkFlags }
newtype VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlags { unVkDescriptorPoolCreateFlags :: VkFlags }
newtype VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlags { unVkDescriptorSetLayoutCreateFlags :: VkFlags }
newtype VkDeviceCreateFlags = VkDeviceCreateFlags { unVkDeviceCreateFlags :: VkFlags }
newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags { unVkDeviceQueueCreateFlags :: VkFlags }
newtype VkFenceCreateFlags = VkFenceCreateFlags { unVkFenceCreateFlags :: VkFlags }
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags { unVkFramebufferCreateFlags :: VkFlags }
newtype VkImageAspectFlags = VkImageAspectFlags { unVkImageAspectFlags :: VkFlags }
newtype VkImageCreateFlags = VkImageCreateFlags { unVkImageCreateFlags :: VkFlags }
newtype VkImageUsageFlags = VkImageUsageFlags { unVkImageUsageFlags :: VkFlags }
newtype VkImageViewCreateFlags = VkImageViewCreateFlags { unVkImageViewCreateFlags :: VkFlags }
newtype VkMemoryMapFlags = VkMemoryMapFlags { unVkMemoryMapFlags :: VkFlags }
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags { unVkPipelineCacheCreateFlags :: VkFlags }
newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags { unVkPipelineColorBlendStateCreateFlags :: VkFlags }
newtype VkPipelineCreateFlags = VkPipelineCreateFlags { unVkPipelineCreateFlags :: VkFlags }
newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags { unVkPipelineDepthStencilStateCreateFlags :: VkFlags }
newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags { unVkPipelineDynamicStateCreateFlags :: VkFlags }
newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags { unVkPipelineInputAssemblyStateCreateFlags :: VkFlags }
newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags { unVkPipelineLayoutCreateFlags :: VkFlags }
newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags { unVkPipelineMultisampleStateCreateFlags :: VkFlags }
newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags { unVkPipelineRasterizationStateCreateFlags :: VkFlags }
newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags { unVkPipelineShaderStageCreateFlags :: VkFlags }
newtype VkPipelineStageFlags = VkPipelineStageFlags { unVkPipelineStageFlags :: VkFlags }
newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags { unVkPipelineTessellationStateCreateFlags :: VkFlags }
newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags { unVkPipelineVertexInputStateCreateFlags :: VkFlags }
newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags { unVkPipelineViewportStateCreateFlags :: VkFlags }
newtype VkQueryControlFlags = VkQueryControlFlags { unVkQueryControlFlags :: VkFlags }
newtype VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlags { unVkQueryPipelineStatisticFlags :: VkFlags }
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags { unVkRenderPassCreateFlags :: VkFlags }
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags { unVkSemaphoreCreateFlags :: VkFlags }
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags { unVkShaderModuleCreateFlags :: VkFlags }
newtype VkShaderStageFlags = VkShaderStageFlags { unVkShaderStageFlags :: VkFlags }
newtype VkSubpassDescriptionFlags = VkSubpassDescriptionFlags { unVkSubpassDescriptionFlags :: VkFlags }
newtype VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagsKHR { unVkSwapchainCreateFlagsKHR :: VkFlags }


-- Vulkan Handles
newtype VkBuffer = VkBuffer { unVkBuffer :: VkHandle }
newtype VkBufferView = VkBufferView { unVkBufferView :: VkHandle }
newtype VkCommandBuffer = VkCommandBuffer { unVkCommandBuffer :: VkHandle }
newtype VkCommandPool = VkCommandPool { unVkCommandPool :: VkHandle }
newtype VkDescriptorPool = VkDescriptorPool { unVkDescriptorPool :: VkHandle }
newtype VkDescriptorSet = VkDescriptorSet { unVkDescriptorSet :: VkHandle }
newtype VkDescriptorSetLayout = VkDescriptorSetLayout { unVkDescriptorSetLayout :: VkHandle }
newtype VkDevice = VkDevice { unVkDevice :: VkHandle }
newtype VkDeviceMemory = VkDeviceMemory { unVkDeviceMemory :: VkHandle }
newtype VkDeviceSize = VkDeviceSize { unVkDeviceSize :: VkHandle }
newtype VkFence = VkFence { unVkFence :: VkHandle }
newtype VkFramebuffer = VkFramebuffer { unVkFramebuffer :: VkHandle }
newtype VkImage = VkImage { unVkImage :: VkHandle }
newtype VkImageView = VkImageView { unVkImageView :: VkHandle }
newtype VkInstance = VkInstance { unVkInstance :: VkHandle }
newtype VkPhysicalDevice = VkPhysicalDevice { unVkPhysicalDevice :: VkHandle }
newtype VkPipeline = VkPipeline { unVkPipeline :: VkHandle }
newtype VkPipelineCache = VkPipelineCache { unVkPipelineCache :: VkHandle }
newtype VkPipelineLayout = VkPipelineLayout { unVkPipelineLayout :: VkHandle }
newtype VkRenderPass = VkRenderPass { unVkRenderPass :: VkHandle }
newtype VkSampler = VkSampler { unVkSampler :: VkHandle }
newtype VkSemaphore = VkSemaphore { unVkSemaphore :: VkHandle }
newtype VkShaderModule = VkShaderModule { unVkShaderModule :: VkHandle }
newtype VkSurfaceKHR = VkSurfaceKHR { unVkSurfaceKHR :: VkHandle }
newtype VkSwapchainKHR = VkSwapchainKHR { unVkSwapchainKHR :: VkHandle }
newtype VkQueue = VkQueue { unVkQueue :: VkHandle }


-- Vulkan function pointers.
type PFN_vkVoidFunction = FunPtr (IO ())
type PFN_vkAllocationFunction = FunPtr (Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkReallocationFunction = FunPtr (Ptr Void -> Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())
type PFN_vkInternalAllocationNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
type PFN_vkInternalFreeNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())


--------------------------------------------------------------------------------------------------------------------------------
--
-- Storable instances for Vulkan types.
--
--------------------------------------------------------------------------------------------------------------------------------
instance Storable VkBool where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBool v)
    poke p (VkBool v) = pokeByteOff p 0 v

instance Storable VkSampleMask where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSampleMask v)
    poke p (VkSampleMask v) = pokeByteOff p 0 v


--------------------------------------------------------------------------------------------------------------------------------
--
-- Storable instances for Vulkan flags.
--
--------------------------------------------------------------------------------------------------------------------------------
instance Storable VkAccessFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkAccessFlags v)
    poke p (VkAccessFlags v) = pokeByteOff p 0 v

instance Storable VkAttachmentDescriptionFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkAttachmentDescriptionFlags v)
    poke p (VkAttachmentDescriptionFlags v) = pokeByteOff p 0 v

instance Storable VkBufferCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferCreateFlags v)
    poke p (VkBufferCreateFlags v) = pokeByteOff p 0 v

instance Storable VkBufferUsageFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferUsageFlags v)
    poke p (VkBufferUsageFlags v) = pokeByteOff p 0 v

instance Storable VkColorComponentFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkColorComponentFlags v)
    poke p (VkColorComponentFlags v) = pokeByteOff p 0 v

instance Storable VkCommandBufferUsageFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkCommandBufferUsageFlags v)
    poke p (VkCommandBufferUsageFlags v) = pokeByteOff p 0 v

instance Storable VkCommandPoolCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkCommandPoolCreateFlags v)
    poke p (VkCommandPoolCreateFlags v) = pokeByteOff p 0 v

instance Storable VkCullModeFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkCullModeFlags v)
    poke p (VkCullModeFlags v) = pokeByteOff p 0 v

instance Storable VkDependencyFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDependencyFlags v)
    poke p (VkDependencyFlags v) = pokeByteOff p 0 v

instance Storable VkDescriptorPoolCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorPoolCreateFlags v)
    poke p (VkDescriptorPoolCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDescriptorSetLayoutCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorSetLayoutCreateFlags v)
    poke p (VkDescriptorSetLayoutCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDeviceCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceCreateFlags v)
    poke p (VkDeviceCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDeviceQueueCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceQueueCreateFlags v)
    poke p (VkDeviceQueueCreateFlags v) = pokeByteOff p 0 v

instance Storable VkFenceCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkFenceCreateFlags v)
    poke p (VkFenceCreateFlags v) = pokeByteOff p 0 v

instance Storable VkFramebufferCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkFramebufferCreateFlags v)
    poke p (VkFramebufferCreateFlags v) = pokeByteOff p 0 v

instance Storable VkImageAspectFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageAspectFlags v)
    poke p (VkImageAspectFlags v) = pokeByteOff p 0 v

instance Storable VkImageCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageCreateFlags v)
    poke p (VkImageCreateFlags v) = pokeByteOff p 0 v

instance Storable VkImageUsageFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageUsageFlags v)
    poke p (VkImageUsageFlags v) = pokeByteOff p 0 v

instance Storable VkImageViewCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageViewCreateFlags v)
    poke p (VkImageViewCreateFlags v) = pokeByteOff p 0 v

instance Storable VkMemoryMapFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkMemoryMapFlags v)
    poke p (VkMemoryMapFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineCacheCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineCacheCreateFlags v)
    poke p (VkPipelineCacheCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineColorBlendStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineColorBlendStateCreateFlags v)
    poke p (VkPipelineColorBlendStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineCreateFlags v)
    poke p (VkPipelineCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineDepthStencilStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineDepthStencilStateCreateFlags v)
    poke p (VkPipelineDepthStencilStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineDynamicStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineDynamicStateCreateFlags v)
    poke p (VkPipelineDynamicStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineInputAssemblyStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineInputAssemblyStateCreateFlags v)
    poke p (VkPipelineInputAssemblyStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineLayoutCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineLayoutCreateFlags v)
    poke p (VkPipelineLayoutCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineMultisampleStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineMultisampleStateCreateFlags v)
    poke p (VkPipelineMultisampleStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineRasterizationStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineRasterizationStateCreateFlags v)
    poke p (VkPipelineRasterizationStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineShaderStageCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineShaderStageCreateFlags v)
    poke p (VkPipelineShaderStageCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineStageFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineStageFlags v)
    poke p (VkPipelineStageFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineTessellationStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineTessellationStateCreateFlags v)
    poke p (VkPipelineTessellationStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineVertexInputStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineVertexInputStateCreateFlags v)
    poke p (VkPipelineVertexInputStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineViewportStateCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineViewportStateCreateFlags v)
    poke p (VkPipelineViewportStateCreateFlags v) = pokeByteOff p 0 v

instance Storable VkQueryControlFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkQueryControlFlags v)
    poke p (VkQueryControlFlags v) = pokeByteOff p 0 v

instance Storable VkQueryPipelineStatisticFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkQueryPipelineStatisticFlags v)
    poke p (VkQueryPipelineStatisticFlags v) = pokeByteOff p 0 v

instance Storable VkRenderPassCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkRenderPassCreateFlags v)
    poke p (VkRenderPassCreateFlags v) = pokeByteOff p 0 v

instance Storable VkSemaphoreCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSemaphoreCreateFlags v)
    poke p (VkSemaphoreCreateFlags v) = pokeByteOff p 0 v

instance Storable VkShaderModuleCreateFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderModuleCreateFlags v)
    poke p (VkShaderModuleCreateFlags v) = pokeByteOff p 0 v

instance Storable VkShaderStageFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderStageFlags v)
    poke p (VkShaderStageFlags v) = pokeByteOff p 0 v

instance Storable VkSubpassDescriptionFlags where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSubpassDescriptionFlags v)
    poke p (VkSubpassDescriptionFlags v) = pokeByteOff p 0 v

instance Storable VkSwapchainCreateFlagsKHR where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSwapchainCreateFlagsKHR v)
    poke p (VkSwapchainCreateFlagsKHR v) = pokeByteOff p 0 v


--------------------------------------------------------------------------------------------------------------------------------
--
-- Storable instances for Vulkan handles.
--
--------------------------------------------------------------------------------------------------------------------------------
instance Storable VkBuffer where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkBuffer v)
    poke p (VkBuffer v) = pokeByteOff p 0 v

instance Storable VkBufferView where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferView v)
    poke p (VkBufferView v) = pokeByteOff p 0 v

instance Storable VkCommandBuffer where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkCommandBuffer v)
    poke p (VkCommandBuffer v) = pokeByteOff p 0 v

instance Storable VkCommandPool where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkCommandPool v)
    poke p (VkCommandPool v) = pokeByteOff p 0 v

instance Storable VkDescriptorPool where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorPool v)
    poke p (VkDescriptorPool v) = pokeByteOff p 0 v

instance Storable VkDescriptorSet where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorSet v)
    poke p (VkDescriptorSet v) = pokeByteOff p 0 v

instance Storable VkDescriptorSetLayout where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorSetLayout v)
    poke p (VkDescriptorSetLayout v) = pokeByteOff p 0 v

instance Storable VkDevice where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDevice v)
    poke p (VkDevice v) = pokeByteOff p 0 v

instance Storable VkDeviceMemory where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceMemory v)
    poke p (VkDeviceMemory v) = pokeByteOff p 0 v

instance Storable VkDeviceSize where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceSize v)
    poke p (VkDeviceSize v) = pokeByteOff p 0 v

instance Storable VkFence where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkFence v)
    poke p (VkFence v) = pokeByteOff p 0 v

instance Storable VkFramebuffer where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkFramebuffer v)
    poke p (VkFramebuffer v) = pokeByteOff p 0 v

instance Storable VkImage where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkImage v)
    poke p (VkImage v) = pokeByteOff p 0 v

instance Storable VkImageView where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkImageView v)
    poke p (VkImageView v) = pokeByteOff p 0 v

instance Storable VkInstance where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkInstance v)
    poke p (VkInstance v) = pokeByteOff p 0 v

instance Storable VkPhysicalDevice where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPhysicalDevice v)
    poke p (VkPhysicalDevice v) = pokeByteOff p 0 v

instance Storable VkPipeline where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPipeline v)
    poke p (VkPipeline v) = pokeByteOff p 0 v

instance Storable VkPipelineCache where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineCache v)
    poke p (VkPipelineCache v) = pokeByteOff p 0 v

instance Storable VkPipelineLayout where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineLayout v)
    poke p (VkPipelineLayout v) = pokeByteOff p 0 v

instance Storable VkRenderPass where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkRenderPass v)
    poke p (VkRenderPass v) = pokeByteOff p 0 v

instance Storable VkSampler where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkSampler v)
    poke p (VkSampler v) = pokeByteOff p 0 v

instance Storable VkSemaphore where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkSemaphore v)
    poke p (VkSemaphore v) = pokeByteOff p 0 v

instance Storable VkShaderModule where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderModule v)
    poke p (VkShaderModule v) = pokeByteOff p 0 v

instance Storable VkSurfaceKHR where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkSurfaceKHR v)
    poke p (VkSurfaceKHR v) = pokeByteOff p 0 v

instance Storable VkSwapchainKHR where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkSwapchainKHR v)
    poke p (VkSwapchainKHR v) = pokeByteOff p 0 v

instance Storable VkQueue where
    sizeOf _    = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkQueue v)
    poke p (VkQueue v) = pokeByteOff p 0 v
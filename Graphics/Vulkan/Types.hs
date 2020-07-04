{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Types where


import Data.Void (Void)
import Data.Word (Word32, Word64)

import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable

import Graphics.Vulkan.Enumerations (VkInternalAllocationType, VkSystemAllocationScope)


-- Vulkan Type aliases.
type VkFlags    = Word32
type VkHandle   = Word64

-- Vulkan Types
newtype VkBool  = VkBool { unVkBool :: Word32 }

-- Vulkan Flags
newtype VkAccessFlags = VkAccessFlags { unVkAccessFlags :: VkFlags }
newtype VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlags { unVkAttachmentDescriptionFlags :: VkFlags }
newtype VkBufferCreateFlags = VkBufferCreateFlags { unVkBufferCreateFlags :: VkFlags }
newtype VkBufferUsageFlags = VkBufferUsageFlags { unVkBufferUsageFlags :: VkFlags }
newtype VkDependencyFlags = VkDependencyFlags { unVkDependencyFlags :: VkFlags }
newtype VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlags { unVkDescriptorPoolCreateFlags :: VkFlags }
newtype VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlags { unVkDescriptorSetLayoutCreateFlags :: VkFlags }
newtype VkDeviceCreateFlags = VkDeviceCreateFlags { unVkDeviceCreateFlags :: VkFlags }
newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags { unVkDeviceQueueCreateFlags :: VkFlags }
newtype VkImageAspectFlags = VkImageAspectFlags { unVkImageAspectFlags :: VkFlags }
newtype VkImageCreateFlags = VkImageCreateFlags { unVkImageCreateFlags :: VkFlags }
newtype VkImageUsageFlags = VkImageUsageFlags { unVkImageUsageFlags :: VkFlags }
newtype VkMemoryMapFlags = VkMemoryMapFlags { unVkMemoryMapFlags :: VkFlags }
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags { unVkPipelineCacheCreateFlags :: VkFlags }
newtype VkPipelineCreateFlags = VkPipelineCreateFlags { unVkPipelineCreateFlags :: VkFlags }
newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags { unVkPipelineLayoutCreateFlags :: VkFlags }
newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags { unVkPipelineShaderStageCreateFlags :: VkFlags }
newtype VkPipelineStageFlags = VkPipelineStageFlags { unVkPipelineStageFlags :: VkFlags }
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags { unVkRenderPassCreateFlags :: VkFlags }
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags { unVkShaderModuleCreateFlags :: VkFlags }
newtype VkShaderStageFlags = VkShaderStageFlags { unVkShaderStageFlags :: VkFlags }
newtype VkSubpassDescriptionFlags = VkSubpassDescriptionFlags { unVkSubpassDescriptionFlags :: VkFlags }


-- Vulkan Handles
newtype VkBuffer = VkBuffer { unVkBuffer :: VkHandle }
newtype VkBufferView = VkBufferView { unVkBufferView :: VkHandle }
newtype VkDescriptorPool = VkDescriptorPool { unVkDescriptorPool :: VkHandle }
newtype VkDescriptorSet = VkDescriptorSet { unVkDescriptorSet :: VkHandle }
newtype VkDescriptorSetLayout = VkDescriptorSetLayout { unVkDescriptorSetLayout :: VkHandle }
newtype VkDevice = VkDevice { unVkDevice :: VkHandle }
newtype VkDeviceMemory = VkDeviceMemory { unVkDeviceMemory :: VkHandle }
newtype VkDeviceSize = VkDeviceSize { unVkDeviceSize :: VkHandle }
newtype VkImage = VkImage { unVkImage :: VkHandle }
newtype VkImageView = VkImageView { unVkImageView :: VkHandle }
newtype VkInstance = VkInstance { unVkInstance :: VkHandle }
newtype VkPhysicalDevice = VkPhysicalDevice { unVkPhysicalDevice :: VkHandle }
newtype VkPipeline = VkPipeline { unVkPipeline :: VkHandle }
newtype VkPipelineCache = VkPipelineCache { unVkPipelineCache :: VkHandle }
newtype VkPipelineLayout = VkPipelineLayout { unVkPipelineLayout :: VkHandle }
newtype VkRenderPass = VkRenderPass { unVkRenderPass :: VkHandle }
newtype VkSampler = VkSampler { unVkSampler :: VkHandle }
newtype VkShaderModule = VkShaderModule { unVkShaderModule :: VkHandle }
newtype VkQueue = VkQueue { unVkQueue :: VkHandle }


-- Vulkan function pointers.
type PFN_vkVoidFunction = FunPtr (IO ())
type PFN_vkAllocationFunction = FunPtr (Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkReallocationFunction = FunPtr (Ptr Void -> Ptr Void -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))
type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())
type PFN_vkInternalAllocationNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
type PFN_vkInternalFreeNotification = FunPtr (Ptr Void -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

-- Storable instances for Vulkan types.
instance Storable VkBool where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBool v)
    poke p (VkBool v) = pokeByteOff p 0 v

-- Storable instances for Vulkan flags.
instance Storable VkAccessFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkAccessFlags v)
    poke p (VkAccessFlags v) = pokeByteOff p 0 v

instance Storable VkAttachmentDescriptionFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkAttachmentDescriptionFlags v)
    poke p (VkAttachmentDescriptionFlags v) = pokeByteOff p 0 v

instance Storable VkBufferCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferCreateFlags v)
    poke p (VkBufferCreateFlags v) = pokeByteOff p 0 v

instance Storable VkBufferUsageFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferUsageFlags v)
    poke p (VkBufferUsageFlags v) = pokeByteOff p 0 v

instance Storable VkDependencyFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDependencyFlags v)
    poke p (VkDependencyFlags v) = pokeByteOff p 0 v

instance Storable VkDescriptorPoolCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorPoolCreateFlags v)
    poke p (VkDescriptorPoolCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDescriptorSetLayoutCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorSetLayoutCreateFlags v)
    poke p (VkDescriptorSetLayoutCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDeviceCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceCreateFlags v)
    poke p (VkDeviceCreateFlags v) = pokeByteOff p 0 v

instance Storable VkDeviceQueueCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceQueueCreateFlags v)
    poke p (VkDeviceQueueCreateFlags v) = pokeByteOff p 0 v

instance Storable VkImageAspectFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageAspectFlags v)
    poke p (VkImageAspectFlags v) = pokeByteOff p 0 v

instance Storable VkImageCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageCreateFlags v)
    poke p (VkImageCreateFlags v) = pokeByteOff p 0 v

instance Storable VkImageUsageFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageUsageFlags v)
    poke p (VkImageUsageFlags v) = pokeByteOff p 0 v

instance Storable VkMemoryMapFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkMemoryMapFlags v)
    poke p (VkMemoryMapFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineCacheCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineCacheCreateFlags v)
    poke p (VkPipelineCacheCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineCreateFlags v)
    poke p (VkPipelineCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineLayoutCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineLayoutCreateFlags v)
    poke p (VkPipelineLayoutCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineShaderStageCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineShaderStageCreateFlags v)
    poke p (VkPipelineShaderStageCreateFlags v) = pokeByteOff p 0 v

instance Storable VkPipelineStageFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineStageFlags v)
    poke p (VkPipelineStageFlags v) = pokeByteOff p 0 v

instance Storable VkRenderPassCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkRenderPassCreateFlags v)
    poke p (VkRenderPassCreateFlags v) = pokeByteOff p 0 v

instance Storable VkShaderModuleCreateFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderModuleCreateFlags v)
    poke p (VkShaderModuleCreateFlags v) = pokeByteOff p 0 v

instance Storable VkShaderStageFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderStageFlags v)
    poke p (VkShaderStageFlags v) = pokeByteOff p 0 v

instance Storable VkSubpassDescriptionFlags where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSubpassDescriptionFlags v)
    poke p (VkSubpassDescriptionFlags v) = pokeByteOff p 0 v

-- Storable instances for Vulkan handles.
instance Storable VkBuffer where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkBuffer v)
    poke p (VkBuffer v) = pokeByteOff p 0 v

instance Storable VkBufferView where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferView v)
    poke p (VkBufferView v) = pokeByteOff p 0 v

instance Storable VkDescriptorPool where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorPool v)
    poke p (VkDescriptorPool v) = pokeByteOff p 0 v

instance Storable VkDescriptorSet where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorSet v)
    poke p (VkDescriptorSet v) = pokeByteOff p 0 v

instance Storable VkDescriptorSetLayout where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDescriptorSetLayout v)
    poke p (VkDescriptorSetLayout v) = pokeByteOff p 0 v

instance Storable VkDevice where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDevice v)
    poke p (VkDevice v) = pokeByteOff p 0 v

instance Storable VkDeviceMemory where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceMemory v)
    poke p (VkDeviceMemory v) = pokeByteOff p 0 v

instance Storable VkDeviceSize where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkDeviceSize v)
    poke p (VkDeviceSize v) = pokeByteOff p 0 v

instance Storable VkImage where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkImage v)
    poke p (VkImage v) = pokeByteOff p 0 v

instance Storable VkImageView where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkImageView v)
    poke p (VkImageView v) = pokeByteOff p 0 v

instance Storable VkInstance where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkInstance v)
    poke p (VkInstance v) = pokeByteOff p 0 v

instance Storable VkPhysicalDevice where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPhysicalDevice v)
    poke p (VkPhysicalDevice v) = pokeByteOff p 0 v

instance Storable VkPipeline where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPipeline v)
    poke p (VkPipeline v) = pokeByteOff p 0 v

instance Storable VkPipelineCache where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineCache v)
    poke p (VkPipelineCache v) = pokeByteOff p 0 v

instance Storable VkPipelineLayout where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkPipelineLayout v)
    poke p (VkPipelineLayout v) = pokeByteOff p 0 v

instance Storable VkRenderPass where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkRenderPass v)
    poke p (VkRenderPass v) = pokeByteOff p 0 v

instance Storable VkSampler where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkSampler v)
    poke p (VkSampler v) = pokeByteOff p 0 v

instance Storable VkShaderModule where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderModule v)
    poke p (VkShaderModule v) = pokeByteOff p 0 v

instance Storable VkQueue where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        v <- peekByteOff p 0
        return (VkQueue v)
    poke p (VkQueue v) = pokeByteOff p 0 v
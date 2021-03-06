{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Descriptor (createVkDescriptorPoolCreateInfo, createVkDescriptorSetAllocateInfo, createVkDescriptorSetLayoutBinding, createVkDescriptorSetLayoutCreateInfo, createVkWriteDescriptorSet, vkAllocateDescriptorSets, vkCreateDescriptorPool, vkCreateDescriptorSetLayout, vkDestroyDescriptorPool, vkDestroyDescriptorSetLayout, vkUpdateDescriptorSets) where


import Data.Maybe
import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type ArrayElement           = Word32
type Binding                = Word32
type BindingCount           = Word32
type DescriptorCopyCount    = Word32
type DescriptorCount        = Word32
type DescriptorSetCount     = Word32
type DescriptorWriteCount   = Word32
type MaxSets                = Word32
type PoolSizeCount          = Word32

foreign import ccall unsafe "vkAllocateDescriptorSets"
    c_vkAllocateDescriptorSets :: VkDevice -> Ptr VkDescriptorSetAllocateInfo -> Ptr VkDescriptorSet -> IO VkResult

foreign import ccall unsafe "vkCreateDescriptorPool"
    c_vkCreateDescriptorPool :: VkDevice -> Ptr VkDescriptorPoolCreateInfo -> Ptr VkAllocationCallbacks ->
        Ptr VkDescriptorPool -> IO VkResult

foreign import ccall unsafe "vkCreateDescriptorSetLayout"
    c_vkCreateDescriptorSetLayout :: VkDevice -> Ptr VkDescriptorSetLayoutCreateInfo -> Ptr VkAllocationCallbacks ->
        Ptr VkDescriptorSetLayout -> IO VkResult

foreign import ccall unsafe "vkDestroyDescriptorPool"
    c_vkDestroyDescriptorPool :: VkDevice -> VkDescriptorPool -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkDestroyDescriptorSetLayout"
    c_vkDestroyDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayout -> Ptr VkAllocationCallbacks
        -> IO ()

foreign import ccall unsafe "vkUpdateDescriptorSets"
    c_vkUpdateDescriptorSets :: VkDevice -> Word32 -> Ptr VkWriteDescriptorSet -> Word32 -> Ptr VkCopyDescriptorSet -> IO ()

createVkDescriptorPoolCreateInfo :: Next -> VkDescriptorPoolCreateFlags -> MaxSets -> PoolSizeCount ->
    [VkDescriptorPoolSize] -> IO VkDescriptorPoolCreateInfo
createVkDescriptorPoolCreateInfo v dPCF mS pSC dPS = allocaArray i $ \p -> do
    pokeArray p dPS
    return $ VkDescriptorPoolCreateInfo structureTypeDescriptorPoolCreateInfo v dPCF mS pSC p
    where
        i = cast pSC

createVkDescriptorSetAllocateInfo :: Next -> VkDescriptorPool -> DescriptorSetCount -> [VkDescriptorSetLayout] ->
    IO VkDescriptorSetAllocateInfo
createVkDescriptorSetAllocateInfo v dP dSC dSL= allocaArray i $ \p -> do
    pokeArray p dSL
    return $ VkDescriptorSetAllocateInfo structureTypeDescriptorSetAllocateInfo v dP dSC p
    where
        i = cast dSC

createVkDescriptorSetLayoutBinding :: Binding -> VkDescriptorType -> DescriptorCount -> [VkShaderStageFlagBits] ->
    Maybe VkSampler -> IO VkDescriptorSetLayoutBinding
createVkDescriptorSetLayoutBinding b dT dC sSFB m = do
    p <- fromMaybeIO m
    return $ VkDescriptorSetLayoutBinding b dT dC sSF p
    where
        sSF = VkShaderStageFlags $ vkBits unVkShaderStageFlagBits sSFB

createVkDescriptorSetLayoutCreateInfo :: Next -> VkDescriptorSetLayoutCreateFlags -> BindingCount ->
    Maybe [VkDescriptorSetLayoutBinding] -> IO VkDescriptorSetLayoutCreateInfo
createVkDescriptorSetLayoutCreateInfo v dSLCF bC dSLB = do
    p <- fromMaybeListIO bC dSLB
    return $ VkDescriptorSetLayoutCreateInfo structureTypeDescriptorSetLayoutCreateInfo v dSLCF bC p

createVkWriteDescriptorSet :: Next -> VkDescriptorSet -> Binding -> ArrayElement -> DescriptorCount -> VkDescriptorType ->
    Maybe VkDescriptorImageInfo -> Maybe VkDescriptorBufferInfo -> Maybe VkBufferView -> IO VkWriteDescriptorSet
createVkWriteDescriptorSet v dS b aE dC dT dII dBI bV = do
    pDII <- fromMaybeIO dII
    pDBI <- fromMaybeIO dBI
    pBV  <- fromMaybeIO bV
    return $ VkWriteDescriptorSet structureTypeWriteDescriptorSet v dS b aE dC dT pDII pDBI pBV

vkAllocateDescriptorSets :: VkDevice -> VkDescriptorSetAllocateInfo -> IO [VkDescriptorSet]
vkAllocateDescriptorSets d dSAI@(VkDescriptorSetAllocateInfo _ _ _ c _) = alloca $ \pDSAI ->
    allocaArray i $ \pDS -> do
        poke pDSAI dSAI
        _ <- c_vkAllocateDescriptorSets d pDSAI pDS
        peekArray i pDS
        where
            i = cast c

vkCreateDescriptorPool :: VkDevice -> VkDescriptorPoolCreateInfo -> IO VkDescriptorPool
vkCreateDescriptorPool d dPCI = alloca $ \pDPCI ->
    alloca $ \pDP -> do
        poke pDPCI dPCI
        _ <- c_vkCreateDescriptorPool d pDPCI nullPtr pDP
        peek pDP

vkCreateDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayoutCreateInfo -> IO VkDescriptorSetLayout
vkCreateDescriptorSetLayout d dSLCI = alloca $ \pDSLCI ->
    alloca $ \pDSL -> do
        poke pDSLCI dSLCI
        _ <- c_vkCreateDescriptorSetLayout d pDSLCI nullPtr pDSL
        peek pDSL

vkDestroyDescriptorPool :: VkDevice -> VkDescriptorPool -> IO ()
vkDestroyDescriptorPool d dP = c_vkDestroyDescriptorPool d dP nullPtr

vkDestroyDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayout -> IO ()
vkDestroyDescriptorSetLayout d dSL = c_vkDestroyDescriptorSetLayout d dSL nullPtr

vkUpdateDescriptorSets :: VkDevice -> DescriptorWriteCount -> Maybe [VkWriteDescriptorSet] -> DescriptorCopyCount ->
    Maybe [VkCopyDescriptorSet] -> IO ()
vkUpdateDescriptorSets d dWC wDS dCC cDS = do
    pWDS <- fromMaybeListIO dWC wDS
    pCDS <- fromMaybeListIO dCC cDS
    c_vkUpdateDescriptorSets d dWC pWDS dCC pCDS
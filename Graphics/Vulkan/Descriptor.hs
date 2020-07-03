{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Descriptor (createVkDescriptorPoolCreateInfo, createVkDescriptorSetAllocateInfo, createVkDescriptorSetLayoutBinding, createVkDescriptorSetLayoutCreateInfo, vkCreateDescriptorPool, vkCreateDescriptorSetLayout) where


import Data.Maybe
import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type Binding                = Word32
type BindingCount           = Word32
type DescriptorCount        = Word32
type DescriptorSetCount     = Word32
type MaxSets                = Word32
type PoolSizeCount          = Word32

foreign import ccall unsafe "vkCreateDescriptorPool"
    c_vkCreateDescriptorPool :: VkDevice -> Ptr VkDescriptorPoolCreateInfo -> Ptr
        VkAllocationCallbacks -> Ptr VkDescriptorPool -> IO VkResult

foreign import ccall unsafe "vkCreateDescriptorSetLayout"
    c_vkCreateDescriptorSetLayout :: VkDevice -> Ptr VkDescriptorSetLayoutCreateInfo -> Ptr
        VkAllocationCallbacks -> Ptr VkDescriptorSetLayout -> IO VkResult

createVkDescriptorPoolCreateInfo :: Ptr Void -> VkDescriptorPoolCreateFlags -> MaxSets -> PoolSizeCount ->
    [VkDescriptorPoolSize] -> IO VkDescriptorPoolCreateInfo
createVkDescriptorPoolCreateInfo v dPCF mS pSC dPS = allocaArray i $ \p -> do
    pokeArray p dPS
    return $ VkDescriptorPoolCreateInfo structureTypeDescriptorPoolCreateInfo v dPCF mS pSC p
    where
        i = cast pSC

createVkDescriptorSetAllocateInfo :: Ptr Void -> VkDescriptorPool -> DescriptorSetCount -> [VkDescriptorSetLayout] ->
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

createVkDescriptorSetLayoutCreateInfo :: Ptr Void -> VkDescriptorSetLayoutCreateFlags -> BindingCount ->
    Maybe [VkDescriptorSetLayoutBinding] -> IO VkDescriptorSetLayoutCreateInfo
createVkDescriptorSetLayoutCreateInfo v dSLCF bC dSLB = do
    p <- fromMaybeListIO bC dSLB
    return $ VkDescriptorSetLayoutCreateInfo structureTypeDescriptorSetLayoutCreateInfo v dSLCF bC p

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
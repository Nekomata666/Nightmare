{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Descriptor (createVkDescriptorSetLayoutBinding, createVkDescriptorSetLayoutCreateInfo, vkCreateDescriptorSetLayout) where


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

foreign import ccall unsafe "vkCreateDescriptorSetLayout"
    c_vkCreateDescriptorSetLayout :: VkDevice -> Ptr VkDescriptorSetLayoutCreateInfo -> Ptr
        VkAllocationCallbacks -> Ptr VkDescriptorSetLayout -> IO VkResult

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

vkCreateDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayoutCreateInfo -> IO VkDescriptorSetLayout
vkCreateDescriptorSetLayout d dSLCI = alloca $ \pDSLCI ->
    alloca $ \pDSL -> do
        poke pDSLCI dSLCI
        _ <- c_vkCreateDescriptorSetLayout d pDSLCI nullPtr pDSL
        peek pDSL
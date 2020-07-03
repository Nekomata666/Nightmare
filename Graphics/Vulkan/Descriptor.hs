{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Descriptor (createVkDescriptorSetLayoutBinding) where


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
type DescriptorCount        = Word32

createVkDescriptorSetLayoutBinding :: Binding -> VkDescriptorType -> DescriptorCount -> [VkShaderStageFlagBits] ->
    Maybe VkSampler -> IO VkDescriptorSetLayoutBinding
createVkDescriptorSetLayoutBinding b dT dC sSFB m = do
    p <- fromMaybeIO m
    return $ VkDescriptorSetLayoutBinding b dT dC sSF p
    where
        sSF = VkShaderStageFlags $ vkBits unVkShaderStageFlagBits sSFB
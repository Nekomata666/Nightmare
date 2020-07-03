{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Pipelines (createPipelineShaderStageInfo, createVkPipelineLayoutCreateInfo, vkCreatePipelineLayout) where


import Data.Maybe
import Data.Void (Void)
import Data.Word (Word32, Word64)

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types

-- Type aliases.
type Name                   = String
type PushConstantRangeCount = Word32
type SetLayoutCount         = Word32

foreign import ccall unsafe "vkCreatePipelineLayout"
    c_vkCreatePipelineLayout :: VkDevice -> Ptr VkPipelineLayoutCreateInfo -> Ptr
        VkAllocationCallbacks -> Ptr VkPipelineLayout -> IO VkResult

createPipelineShaderStageInfo :: Ptr Void -> VkPipelineShaderStageCreateFlags -> VkShaderStageFlagBits -> VkShaderModule ->
    Name -> Maybe VkSpecializationInfo -> IO VkPipelineShaderStageCreateInfo
createPipelineShaderStageInfo v pSSCF sSF sM n mVKSI = do
    n' <- newCString n
    pInfo <- fromMaybeIO mVKSI
    return $ VkPipelineShaderStageCreateInfo structureTypePipelineShaderStageCreateInfo v pSSCF sSF sM n' pInfo

createVkPipelineLayoutCreateInfo :: Ptr Void -> VkPipelineLayoutCreateFlags -> SetLayoutCount -> [VkDescriptorSetLayout] ->
    PushConstantRangeCount -> Maybe [VkPushConstantRange] -> IO VkPipelineLayoutCreateInfo
createVkPipelineLayoutCreateInfo v pLCF sLC dSL pCRC m = allocaArray i $ \pDSL -> do
        pokeArray pDSL dSL
        pPCR <- fromMaybeListIO pCRC m
        return $ VkPipelineLayoutCreateInfo structureTypePipelineLayoutCreateInfo v pLCF sLC pDSL pCRC pPCR
        where
            i = cast sLC

vkCreatePipelineLayout :: VkDevice -> VkPipelineLayoutCreateInfo -> IO VkPipelineLayout
vkCreatePipelineLayout d pLCI = alloca $ \pPLCI ->
    alloca $ \pPL -> do
        poke pPLCI pLCI
        _ <- c_vkCreatePipelineLayout d pPLCI nullPtr pPL
        peek pPL
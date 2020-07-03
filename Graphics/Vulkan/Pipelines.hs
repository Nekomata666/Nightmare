{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Pipelines (createVkPipelineShaderStageInfo, createVkPipelineCacheInfo, createVkPipelineLayoutCreateInfo, vkCreateComputePipelines, vkCreatePipelineCache, vkCreatePipelineLayout) where


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
type CreateInfoCount        = Word32
type Name                   = String
type PushConstantRangeCount = Word32
type SetLayoutCount         = Word32

foreign import ccall unsafe "vkCreateComputePipelines"
    c_vkCreateComputePipelines :: VkDevice -> VkPipelineCache -> Word32 -> Ptr VkComputePipelineCreateInfo ->
        Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult

foreign import ccall unsafe "vkCreatePipelineCache"
    c_vkCreatePipelineCache :: VkDevice -> Ptr VkPipelineCacheCreateInfo -> Ptr VkAllocationCallbacks ->
        Ptr VkPipelineCache -> IO VkResult

foreign import ccall unsafe "vkCreatePipelineLayout"
    c_vkCreatePipelineLayout :: VkDevice -> Ptr VkPipelineLayoutCreateInfo -> Ptr VkAllocationCallbacks ->
        Ptr VkPipelineLayout -> IO VkResult

createVkPipelineCacheInfo :: Ptr Void -> VkPipelineCacheCreateFlags -> FilePath -> IO VkPipelineCacheCreateInfo
createVkPipelineCacheInfo v pCCF fP
    | null fP = return $ VkPipelineCacheCreateInfo structureTypePipelineCacheCreateInfo v pCCF (CSize 0) nullPtr
    | otherwise  = do
        r <- openVulkanFile "CreatePipelineCacheInfo" fP
        let p  = fst r :: Ptr Word32
            cs = snd r
        return $ VkPipelineCacheCreateInfo structureTypePipelineCacheCreateInfo v pCCF cs (castPtr p)

createVkPipelineLayoutCreateInfo :: Ptr Void -> VkPipelineLayoutCreateFlags -> SetLayoutCount -> [VkDescriptorSetLayout] ->
    PushConstantRangeCount -> Maybe [VkPushConstantRange] -> IO VkPipelineLayoutCreateInfo
createVkPipelineLayoutCreateInfo v pLCF sLC dSL pCRC m = allocaArray i $ \pDSL -> do
        pokeArray pDSL dSL
        pPCR <- fromMaybeListIO pCRC m
        return $ VkPipelineLayoutCreateInfo structureTypePipelineLayoutCreateInfo v pLCF sLC pDSL pCRC pPCR
        where
            i = cast sLC

createVkPipelineShaderStageInfo :: Ptr Void -> VkPipelineShaderStageCreateFlags -> VkShaderStageFlagBits -> VkShaderModule ->
    Name -> Maybe VkSpecializationInfo -> IO VkPipelineShaderStageCreateInfo
createVkPipelineShaderStageInfo v pSSCF sSF sM n mVKSI = do
    n' <- newCString n
    pInfo <- fromMaybeIO mVKSI
    return $ VkPipelineShaderStageCreateInfo structureTypePipelineShaderStageCreateInfo v pSSCF sSF sM n' pInfo

vkCreateComputePipelines :: VkDevice -> VkPipelineCache -> CreateInfoCount -> [VkComputePipelineCreateInfo] -> IO [VkPipeline]
vkCreateComputePipelines d pC cIC cPCI = allocaArray i $ \pCPCI ->
    allocaArray i $ \pP -> do
        pokeArray pCPCI cPCI
        _ <- c_vkCreateComputePipelines d pC cIC pCPCI nullPtr pP
        peekArray i pP
        where
            i = cast cIC

vkCreatePipelineCache :: VkDevice -> VkPipelineCacheCreateInfo -> IO VkPipelineCache
vkCreatePipelineCache d pCCI = alloca $ \pPCCI ->
    alloca $ \pPC -> do
        poke pPCCI pCCI
        _ <- c_vkCreatePipelineCache d pPCCI nullPtr pPC
        peek pPC

vkCreatePipelineLayout :: VkDevice -> VkPipelineLayoutCreateInfo -> IO VkPipelineLayout
vkCreatePipelineLayout d pLCI = alloca $ \pPLCI ->
    alloca $ \pPL -> do
        poke pPLCI pLCI
        _ <- c_vkCreatePipelineLayout d pPLCI nullPtr pPL
        peek pPL
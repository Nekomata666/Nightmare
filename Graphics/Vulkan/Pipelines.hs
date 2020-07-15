{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Pipelines (createVkPipelineCacheInfo, createVkPipelineColorBlendStateCreateInfo, createVkPipelineDynamicStateCreateInfo, createVkPipelineLayoutCreateInfo, createVkPipelineShaderStageInfo, createVkPipelineVertexInputStateCreateInfo, createVkPipelineViewportStateCreateInfo, vkCreateComputePipelines, vkCreatePipelineCache, vkCreatePipelineLayout, vkDestroyPipeline, vkDestroyPipelineCache, vkDestroyPipelineLayout) where


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
type AttachmentCount                    = Word32
type BlendConstants                     = [Float]
type CreateInfoCount                    = Word32
type DynamicStateCount                  = Word32
type LogicOpEnable                      = VkBool
type Name                               = String
type PushConstantRangeCount             = Word32
type ScissorCount                       = Word32
type SetLayoutCount                     = Word32
type VertexAttributeDescriptionCount    = Word32
type VertexBindingDescriptionCount      = Word32
type ViewportCount                      = Word32

foreign import ccall unsafe "vkCreateComputePipelines"
    c_vkCreateComputePipelines :: VkDevice -> VkPipelineCache -> Word32 -> Ptr VkComputePipelineCreateInfo ->
        Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult

foreign import ccall unsafe "vkCreatePipelineCache"
    c_vkCreatePipelineCache :: VkDevice -> Ptr VkPipelineCacheCreateInfo -> Ptr VkAllocationCallbacks ->
        Ptr VkPipelineCache -> IO VkResult

foreign import ccall unsafe "vkCreatePipelineLayout"
    c_vkCreatePipelineLayout :: VkDevice -> Ptr VkPipelineLayoutCreateInfo -> Ptr VkAllocationCallbacks ->
        Ptr VkPipelineLayout -> IO VkResult

foreign import ccall unsafe "vkDestroyPipeline"
    c_vkDestroyPipeline :: VkDevice -> VkPipeline -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkDestroyPipelineCache"
    c_vkDestroyPipelineCache :: VkDevice -> VkPipelineCache -> Ptr VkAllocationCallbacks -> IO ()

foreign import ccall unsafe "vkDestroyPipelineLayout"
    c_vkDestroyPipelineLayout :: VkDevice -> VkPipelineLayout -> Ptr VkAllocationCallbacks -> IO ()

createVkPipelineCacheInfo :: Ptr Void -> VkPipelineCacheCreateFlags -> FilePath -> IO VkPipelineCacheCreateInfo
createVkPipelineCacheInfo v pCCF fP
    | null fP = return $ VkPipelineCacheCreateInfo structureTypePipelineCacheCreateInfo v pCCF (CSize 0) nullPtr
    | otherwise  = do
        r <- openVulkanFile fP
        let p  = fst r
            cs = snd r
        return $ VkPipelineCacheCreateInfo structureTypePipelineCacheCreateInfo v pCCF cs (castPtr p)

createVkPipelineColorBlendStateCreateInfo :: Ptr Void -> VkPipelineColorBlendStateCreateFlags -> LogicOpEnable -> VkLogicOp ->
    AttachmentCount -> [VkPipelineColorBlendAttachmentState] -> BlendConstants -> IO VkPipelineColorBlendStateCreateInfo
createVkPipelineColorBlendStateCreateInfo v pCBSCF lOE lO aC pCBAS bC = allocaArray i $ \p ->
    allocaArray 4 $ \pBC -> do
        pokeArray p pCBAS
        pokeArray pBC bC
        return $ VkPipelineColorBlendStateCreateInfo structureTypePipelineColorBlendStateCreateInfo v pCBSCF lOE lO aC p pBC
        where
            i = cast aC

createVkPipelineDynamicStateCreateInfo :: Ptr Void -> VkPipelineDynamicStateCreateFlags -> DynamicStateCount -> [VkDynamicState] -> IO VkPipelineDynamicStateCreateInfo
createVkPipelineDynamicStateCreateInfo v pDSCF dSC dS = allocaArray i $ \pDS -> do
    pokeArray pDS dS
    return $ VkPipelineDynamicStateCreateInfo structureTypePipelineDynamicStateCreateInfo v pDSCF dSC pDS
    where
        i = cast dSC

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

createVkPipelineVertexInputStateCreateInfo :: Ptr Void -> VkPipelineVertexInputStateCreateFlags -> VertexBindingDescriptionCount ->
    Maybe [VkVertexInputBindingDescription] -> VertexAttributeDescriptionCount -> Maybe [VkVertexInputAttributeDescription] ->
    IO VkPipelineVertexInputStateCreateInfo
createVkPipelineVertexInputStateCreateInfo v pVISCF vBDC mVIBD vADC mVIAD = do
    pVIBD <- fromMaybeListIO vBDC mVIBD
    pVIAD <- fromMaybeListIO vADC mVIAD
    return $ VkPipelineVertexInputStateCreateInfo structureTypePipelineVertexInputStateCreateInfo v pVISCF vBDC pVIBD vADC pVIAD

createVkPipelineViewportStateCreateInfo :: Ptr Void -> VkPipelineViewportStateCreateFlags -> ViewportCount -> [VkViewport] ->
    ScissorCount -> [VkRect2D] -> IO VkPipelineViewportStateCreateInfo
createVkPipelineViewportStateCreateInfo v pVSCF vC vVs sC sS = allocaArray i1 $ \pVVs ->
    allocaArray i2 $ \pSSs -> do
        pokeArray pVVs vVs
        pokeArray pSSs sS
        return $ VkPipelineViewportStateCreateInfo structureTypePipelineViewportStateCreateInfo v pVSCF vC pVVs sC pSSs
        where
            i1 = cast vC
            i2 = cast sC

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

vkDestroyPipeline :: VkDevice -> VkPipeline -> IO ()
vkDestroyPipeline d p = c_vkDestroyPipeline d p nullPtr

vkDestroyPipelineCache :: VkDevice -> VkPipelineCache -> IO ()
vkDestroyPipelineCache d pC = c_vkDestroyPipelineCache d pC nullPtr

vkDestroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
vkDestroyPipelineLayout d pL = c_vkDestroyPipelineLayout d pL nullPtr
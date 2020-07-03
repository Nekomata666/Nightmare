{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Pipelines (createPipelineShaderStageInfo) where


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

createPipelineShaderStageInfo :: Ptr Void -> VkPipelineShaderStageCreateFlags -> VkShaderStageFlagBits -> VkShaderModule ->
    Name -> Maybe VkSpecializationInfo -> IO VkPipelineShaderStageCreateInfo
createPipelineShaderStageInfo v pSSCF sSF sM n Nothing = do
    n' <- newCString n
    return $ VkPipelineShaderStageCreateInfo structureTypePipelineShaderStageCreateInfo v pSSCF sSF sM n' nullPtr
createPipelineShaderStageInfo v pSSCF sSF sM n (Just m) = alloca $ \pInfo -> do
    n' <- newCString n
    poke pInfo m
    return $ VkPipelineShaderStageCreateInfo structureTypePipelineShaderStageCreateInfo v pSSCF sSF sM n' pInfo
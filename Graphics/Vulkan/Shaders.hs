{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Shaders (createShaderModuleInfo) where


import Data.Void (Void)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


createShaderModuleInfo :: Ptr Void -> VkShaderModuleCreateFlags -> FilePath -> IO VkShaderModuleCreateInfo
createShaderModuleInfo v sMCF fP = do
    r <- openVulkanFile "CreateShaderModuleInfo" fP
    let p  = fst r
        cs = snd r
    return $ VkShaderModuleCreateInfo structureTypeShaderModuleCreateInfo v sMCF cs p
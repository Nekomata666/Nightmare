{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Shaders (createVkShaderModuleInfo, vkCreateShaderModule, vkDestroyShaderModule) where


import Data.Void (Void)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


foreign import ccall unsafe "vkCreateShaderModule"
    c_vkCreateShaderModule :: VkDevice -> Ptr VkShaderModuleCreateInfo -> Ptr VkAllocationCallbacks
        -> Ptr VkShaderModule -> IO VkResult

foreign import ccall unsafe "vkDestroyShaderModule"
    c_vkDestroyShaderModule :: VkDevice -> VkShaderModule -> Ptr VkAllocationCallbacks -> IO ()

createVkShaderModuleInfo :: Ptr Void -> VkShaderModuleCreateFlags -> FilePath -> IO VkShaderModuleCreateInfo
createVkShaderModuleInfo v sMCF fP = do
    r <- openVulkanFile fP
    let p  = fst r
        cs = snd r
    return $ VkShaderModuleCreateInfo structureTypeShaderModuleCreateInfo v sMCF cs p

vkCreateShaderModule :: VkDevice -> VkShaderModuleCreateInfo -> IO VkShaderModule
vkCreateShaderModule d sMCI = alloca $ \pSMCI ->
    alloca $ \pSM -> do
        poke pSMCI sMCI
        _ <- c_vkCreateShaderModule d pSMCI nullPtr pSM
        peek pSM

vkDestroyShaderModule :: VkDevice -> VkShaderModule -> IO ()
vkDestroyShaderModule d sM = c_vkDestroyShaderModule d sM nullPtr
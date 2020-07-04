{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Renderpass (createVkRenderPassCreateInfo) where


import Data.Maybe (Maybe)
import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type AttachmentCount            = Word32
type SubpassCount               = Word32
type DependencyCount            = Word32


createVkRenderPassCreateInfo :: Ptr Void -> VkRenderPassCreateFlags -> AttachmentCount -> Maybe [VkAttachmentDescription] ->
    SubpassCount -> Maybe [VkSubpassDescription] -> DependencyCount -> Maybe [VkSubpassDependency] -> IO VkRenderPassCreateInfo
createVkRenderPassCreateInfo v rPCF aC aD sC sDes dC sDep = do
    aD'   <- fromMaybeListIO aC aD
    sDes' <- fromMaybeListIO sC sDes
    sDep' <- fromMaybeListIO dC sDep
    return $ VkRenderPassCreateInfo structureTypeRenderPassCreateInfo v rPCF aC aD' sC sDes' dC sDep'
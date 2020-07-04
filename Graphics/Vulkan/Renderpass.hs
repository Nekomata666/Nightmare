{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Renderpass (createVkRenderPassCreateInfo, createVkSubpassDescription) where


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
type ColorAttachment            = VkAttachmentReference
type ColorAttachmentCount       = Word32
type DependencyCount            = Word32
type DepthStencilAttachment     = VkAttachmentReference
type InputAttachment            = VkAttachmentReference
type InputAttachmentCount       = Word32
type ResolveAttachment          = VkAttachmentReference
type PreserveAttachment         = Word32
type PreserveAttachmentCount    = Word32
type SubpassCount               = Word32


foreign import ccall unsafe "vkCreateRenderPass"
    c_vkCreateRenderPass :: VkDevice -> Ptr VkRenderPassCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkRenderPass ->
        IO VkResult

createVkRenderPassCreateInfo :: Ptr Void -> VkRenderPassCreateFlags -> AttachmentCount -> Maybe [VkAttachmentDescription] ->
    SubpassCount -> Maybe [VkSubpassDescription] -> DependencyCount -> Maybe [VkSubpassDependency] -> IO VkRenderPassCreateInfo
createVkRenderPassCreateInfo v rPCF aC aD sC sDes dC sDep = do
    aD'   <- fromMaybeListIO aC aD
    sDes' <- fromMaybeListIO sC sDes
    sDep' <- fromMaybeListIO dC sDep
    return $ VkRenderPassCreateInfo structureTypeRenderPassCreateInfo v rPCF aC aD' sC sDes' dC sDep'

createVkSubpassDescription :: VkSubpassDescriptionFlagBits -> VkPipelineBindPoint -> InputAttachmentCount ->
    Maybe [InputAttachment] -> ColorAttachmentCount -> Maybe [ColorAttachment] -> Maybe [ResolveAttachment] ->
    Maybe DepthStencilAttachment -> PreserveAttachmentCount -> Maybe [PreserveAttachment] -> IO VkSubpassDescription
createVkSubpassDescription sDFB pBP iAC iA cAC cA rA dSA pAC pA = do
    iA'  <- fromMaybeListIO iAC iA
    cA'  <- fromMaybeListIO cAC cA
    rA'  <- fromMaybeListIO cAC rA
    dSA' <- fromMaybeIO dSA
    pA'  <- fromMaybeListIO pAC pA
    return $ VkSubpassDescription f pBP iAC iA' cAC cA' rA' dSA' pAC pA'
        where
            f = VkSubpassDescriptionFlags $ unVkSubpassDescriptionFlagBits sDFB
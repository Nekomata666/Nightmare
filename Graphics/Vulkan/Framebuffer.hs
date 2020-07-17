{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Framebuffer (createVkFramebufferCreateInfo, vkCreateFramebuffer) where


import Data.Maybe   (Maybe)
import Data.Void    (Void)
import Data.Word    (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type AttachmentCount    = Word32
type Width              = Word32
type Height             = Word32
type Layers             = Word32


foreign import ccall unsafe "vkCreateFramebuffer"
    c_vkCreateFramebuffer :: VkDevice -> Ptr VkFramebufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFramebuffer -> IO VkResult

createVkFramebufferCreateInfo :: Ptr Void -> VkFramebufferCreateFlags -> VkRenderPass -> AttachmentCount -> [VkImageView] ->
    Width -> Height -> Layers -> IO VkFramebufferCreateInfo
createVkFramebufferCreateInfo v fCF rP aC iV w h l = allocaArray i $ \p -> do
    pokeArray p iV
    return $ VkFramebufferCreateInfo structureTypeFramebufferCreateInfo v fCF rP aC p w h l
    where
        i = cast aC

vkCreateFramebuffer :: VkDevice -> VkFramebufferCreateInfo -> IO VkFramebuffer
vkCreateFramebuffer d fCI = alloca $ \pFCI ->
    alloca $ \pF -> do
        poke pFCI fCI
        _ <- c_vkCreateFramebuffer d pFCI nullPtr pF
        peek pF
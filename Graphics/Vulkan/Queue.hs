{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Queue (createVkSubmitInfo, vkQueueSubmit) where


import Control.DeepSeq
import Control.Exception.Base

import Data.Maybe
import Data.Void (Void)
import Data.Word (Word32)

import Foreign

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type SubmitCount        = Word32

foreign import ccall unsafe "vkQueueSubmit"
    c_vkQueueSubmit :: VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult

createVkSubmitInfo :: Ptr Void -> Word32 -> Maybe [VkSemaphore] -> Maybe [VkPipelineStageFlags] -> Word32 -> [VkCommandBuffer] ->
    Word32 -> Maybe [VkSemaphore] -> IO VkSubmitInfo
createVkSubmitInfo v wSC wS f cBC cB sSC sS = allocaArray cBI $ \pCB -> do
    pWS <- fromMaybeListIO wSC wS
    pF  <- fromMaybeListIO wSC f
    pokeArray pCB cB
    pSS <- fromMaybeListIO sSC sS
    return $ VkSubmitInfo structureTypeSubmitInfo v wSC pWS pF cBC pCB sSC pSS
    where
        cBI = cast cBC

vkQueueSubmit :: VkQueue -> SubmitCount -> [VkSubmitInfo] -> VkFence -> IO VkResult
vkQueueSubmit q c info f = allocaArray i $ \p -> do
    pokeArray p info
    c_vkQueueSubmit q c p f
    where
        i = cast c
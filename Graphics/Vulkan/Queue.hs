{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Queue (createVkSubmitInfo) where


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
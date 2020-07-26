{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Queue (createVkPresentInfoKHR, createVkSubmitInfo, vkQueueSubmit, vkQueueWaitIdle) where


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
type CommandBufferCount     = Word32
type ImageIndices           = Word32
type SignalSemaphoreCount   = Word32
type SubmitCount            = Word32
type SwapchainCount         = Word32
type WaitSemaphoreCount     = Word32

foreign import ccall unsafe "vkQueueSubmit"
    c_vkQueueSubmit :: VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult

foreign import ccall unsafe "vkQueueWaitIdle"
    c_vkQueueWaitIdle :: VkQueue -> IO VkResult

createVkPresentInfoKHR :: Next -> WaitSemaphoreCount -> [VkSemaphore] -> SwapchainCount -> [VkSwapchainKHR] -> [ImageIndices] -> IO VkPresentInfoKHR
createVkPresentInfoKHR v wSC wS sC s iI = allocaArray iWSC $ \pWS ->
    allocaArray iSC $ \pS ->
        allocaArray iSC $ \pII ->
            allocaArray iSC $ \pR -> do
                pokeArray pWS wS
                pokeArray pS s
                pokeArray pII iI
                return $ VkPresentInfoKHR structureTypePresentInfoKHR v wSC pWS sC pS pII pR
                where
                    iWSC = cast wSC
                    iSC  = cast sC

createVkSubmitInfo :: Next -> WaitSemaphoreCount -> Maybe [VkSemaphore] -> Maybe [VkPipelineStageFlagBits] ->
    CommandBufferCount -> [VkCommandBuffer] -> SignalSemaphoreCount -> Maybe [VkSemaphore] -> IO VkSubmitInfo
createVkSubmitInfo v wSC wS fB cBC cB sSC sS = allocaArray cBI $ \pCB -> do
    pWS <- fromMaybeListIO wSC wS
    pF  <- fromMaybeListIO wSC f
    pokeArray pCB cB
    pSS <- fromMaybeListIO sSC sS
    return $ VkSubmitInfo structureTypeSubmitInfo v wSC pWS pF cBC pCB sSC pSS
    where
        cBI = cast cBC
        f = Just $ map (VkPipelineStageFlags . unVkPipelineStageFlagBits) $ fromMaybe [VkPipelineStageFlagBits 0] fB

vkQueueSubmit :: VkQueue -> SubmitCount -> [VkSubmitInfo] -> VkFence -> IO VkResult
vkQueueSubmit q c info f = allocaArray i $ \p -> do
    pokeArray p info
    c_vkQueueSubmit q c p f
    where
        i = cast c

vkQueueWaitIdle :: VkQueue -> IO VkResult
vkQueueWaitIdle = c_vkQueueWaitIdle
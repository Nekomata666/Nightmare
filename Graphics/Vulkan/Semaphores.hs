{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Semaphores (createVkSemaphoreCreateInfo, createVkSemaphoreTypeCreateInfo, vkCreateSemaphore, vkDestroySemaphore) where


import Data.Void (Void)
import Data.Word (Word64)

import Foreign

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type InitialValue   = Word64


foreign import ccall unsafe "vkCreateSemaphore"
    c_vkCreateSemaphore :: VkDevice -> Ptr VkSemaphoreCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSemaphore -> IO VkResult

foreign import ccall unsafe "vkDestroySemaphore"
    c_vkDestroySemaphore :: VkDevice -> VkSemaphore -> Ptr VkAllocationCallbacks -> IO ()

createVkSemaphoreTypeCreateInfo :: Next -> VkSemaphoreType -> InitialValue -> IO VkSemaphoreCreateInfo
createVkSemaphoreTypeCreateInfo v sT iV = alloca $ \p -> do
    poke p t
    createVkSemaphoreCreateInfo (castPtr p) $ VkSemaphoreCreateFlags 0
    where
        t = VkSemaphoreTypeCreateInfo structureTypeSemaphoreTypeCreateInfo v sT iV

createVkSemaphoreCreateInfo :: Next -> VkSemaphoreCreateFlags -> IO VkSemaphoreCreateInfo
createVkSemaphoreCreateInfo v sCF = return $ VkSemaphoreCreateInfo structureTypeSemaphoreCreateInfo v sCF

vkCreateSemaphore :: VkDevice -> VkSemaphoreCreateInfo -> IO VkSemaphore
vkCreateSemaphore d sCI = alloca $ \pSCI ->
    alloca $ \pS -> do
        poke pSCI sCI
        _ <- c_vkCreateSemaphore d pSCI nullPtr pS
        peek pS

vkDestroySemaphore :: VkDevice -> VkSemaphore -> IO ()
vkDestroySemaphore d s = c_vkDestroySemaphore d s nullPtr
{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Semaphores (createVkSemaphoreCreateInfo) where


import Data.Void (Void)
import Data.Word (Word64)

import Foreign

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


createVkSemaphoreCreateInfo :: Next -> VkSemaphoreCreateFlags -> IO VkSemaphoreCreateInfo
createVkSemaphoreCreateInfo v sCF = return $ VkSemaphoreCreateInfo structureTypeSemaphreCreateInfo v sCF
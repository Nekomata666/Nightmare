{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Instance (createVkApplicationInfo, createVkInstanceCreateInfo, vkCreateInstance, vkDestroyInstance) where


import Data.Void (Void)
import Data.Word (Word32)

import Foreign
import Foreign.C.String

import Graphics.Utilities

import Graphics.Vulkan.Data
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Types


-- Type aliases.
type APIVersion         = Word32
type ApplicationName    = String
type ApplicationVersion = Word32
type EngineName         = String
type EngineVersion      = Word32
type ExtensionCount     = Word32
type ExtensionNames     = [String]
type LayerCount         = Word32
type LayerNames         = [String]


foreign import ccall unsafe "vkCreateInstance"
    c_vkCreateInstance :: Ptr VkInstanceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult

foreign import ccall unsafe "vkDestroyInstance"
    c_vkDestroyInstance :: VkInstance -> Ptr VkAllocationCallbacks -> IO ()

createVkApplicationInfo :: Next -> ApplicationName -> ApplicationVersion -> EngineName -> EngineVersion -> APIVersion ->
    IO VkApplicationInfo
createVkApplicationInfo v aN appV eN eV apiV = do
    aN' <- newCString aN
    eN' <- newCString eN
    return $ VkApplicationInfo structureTypeApplicationInfo v aN' appV eN' eV apiV

createVkInstanceCreateInfo :: Next -> VkFlags -> Maybe VkApplicationInfo -> LayerCount -> Maybe LayerNames -> ExtensionCount ->
    Maybe ExtensionNames -> IO VkInstanceCreateInfo
createVkInstanceCreateInfo v f aI lC lN eC eN = do
    aI' <- fromMaybeIO aI
    lN' <- fromMaybeStringListIO lC lN
    eN' <- fromMaybeStringListIO eC eN
    return $ VkInstanceCreateInfo structureTypeInstanceCreateInfo v f aI' lC lN' eC eN'

vkCreateInstance :: VkInstanceCreateInfo -> IO VkInstance
vkCreateInstance vkInfo = alloca $ \pVkInfo ->
    alloca $ \pVkInstance -> do
        poke pVkInfo vkInfo
        _ <- c_vkCreateInstance pVkInfo nullPtr pVkInstance
        peek pVkInstance

vkDestroyInstance :: VkInstance -> IO ()
vkDestroyInstance i = c_vkDestroyInstance i nullPtr
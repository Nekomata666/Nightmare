{-# LANGUAGE ForeignFunctionInterface, Safe #-}

module Graphics.Vulkan.Instance (createVkApplicationInfo, createVkInstanceCreateInfo, vkCreateInstance) where


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

createVkApplicationInfo :: Ptr Void -> ApplicationName -> ApplicationVersion -> EngineName -> EngineVersion -> APIVersion ->
    IO VkApplicationInfo
createVkApplicationInfo v aN appV eN eV apiV = do
    aN' <- newCString aN
    eN' <- newCString eN
    return $ VkApplicationInfo structureTypeApplicationInfo v aN' appV eN' eV apiV

createVkInstanceCreateInfo :: Ptr Void -> VkFlags -> Maybe VkApplicationInfo -> LayerCount -> Maybe LayerNames ->
    ExtensionCount -> Maybe ExtensionNames -> IO VkInstanceCreateInfo
createVkInstanceCreateInfo v f aI lC lN eC eN = do
    aI' <- fromMaybeIO aI
    lN' <- fromMaybeStringListIO lC lN
    eN' <- fromMaybeStringListIO eC eN
    return $ VkInstanceCreateInfo structureTypeInstanceCreateInfo v f aI' lC lN' eC eN'

vkCreateInstance :: VkInstanceCreateInfo -> IO (VkInstance, VkResult)
vkCreateInstance vkInfo = alloca $ \pVkInfo ->
    alloca $ \pVkInstance -> do
        poke pVkInfo vkInfo
        vkR <- c_vkCreateInstance pVkInfo nullPtr pVkInstance
        inst <- peek pVkInstance
        return (inst, vkR)

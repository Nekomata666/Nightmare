{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)

import Data.ByteString (ByteString, hPut, pack)
import Data.Word (Word32)

import Foreign (Storable, allocaArray, pokeArray)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (castPtr, nullPtr)

import GHC.IO.Handle.Text (memcpy)

import Graphics.Utilities

import Graphics.Vulkan.Buffers
import Graphics.Vulkan.Command
import Graphics.Vulkan.Constants
import Graphics.Vulkan.Data (VkBufferCopy(VkBufferCopy), VkCommandBufferBeginInfo, VkComponentMapping(..), VkDescriptorPoolSize(..), VkExtent2D(..), VkFenceCreateInfo(..), VkImageSubresourceRange, VkImageViewCreateInfo(..), VkMemoryRequirements(..))
import Graphics.Vulkan.Devices -- (createVkDeviceCreateInfo, vkCreateDevice)
import Graphics.Vulkan.Descriptor
import Graphics.Vulkan.Enumerations
import Graphics.Vulkan.Fences
import Graphics.Vulkan.Images
import Graphics.Vulkan.Instance
import Graphics.Vulkan.Memory
import Graphics.Vulkan.Queue
import Graphics.Vulkan.Semaphores
import Graphics.Vulkan.Surface (createVkSwapchainCreateInfo, vkAcquireNextImageKHR, vkCreateSwapchainKHR, vkGetSwapchainImagesKHR)
import Graphics.Vulkan.Types

import Math.Data


-- Type aliases.
type Frame  = Int
type ResX   = Word32
type ResY   = Word32
type SSAA   = Word32

frameTime :: Float
frameTime = 1/200

swapChainCount :: Word32
swapChainCount = 3

vertex2ToList :: Num a => Vertex2 a -> [a]
vertex2ToList (Vertex2 x y)             = [x, y]
vertex3ToList :: Num a => Vertex3 a -> [a]
vertex3ToList (Vertex3 x y z)           = [x, y, z]
vertex4ToList :: Num a => Vertex4 a -> [a]
vertex4ToList (Vertex4 x y z w)         = [x, y, z, w]

uboToList :: Num a => UBO a -> [a]
uboToList (UBO (Quaternion x1 y1 z1 w1) (Quaternion x2 y2 z2 w2)) = [x1, y1, z1, w1, x2, y2, z2, w2]

allocateCommandBuffer :: VkDevice -> VkCommandPool -> VkCommandBufferLevel -> IO VkCommandBuffer
allocateCommandBuffer vkDev0 cmdPo0 lvl = do
    vkCoBu  <- vkAllocateCommandBuffers vkDev0 vkCBAI
    return $ head vkCoBu
    where
        vkCBAI = createVkCommandBufferAllocateInfo nullPtr cmdPo0 lvl 1

copyBuffer :: VkDevice -> VkBuffer -> VkBuffer -> VkCommandPool -> VkDeviceSize -> VkQueue -> IO ()
copyBuffer vkDev0 src dst cmdPo0 s q = do
    cmdBuf  <- allocateCommandBuffer vkDev0 cmdPo0 commandBufferLevelPrimary
    cmdBBI  <- createVkCommandBufferBeginInfo nullPtr commandBufferUsageOneSubmitBit Nothing
    _ <- vkBeginCommandBuffer cmdBuf cmdBBI
    vkCmdCopyBuffer cmdBuf src dst 1 [buffCo]
    _ <- vkEndCommandBuffer cmdBuf
    vkSuIn <- createVkSubmitInfo nullPtr 0 Nothing Nothing 1 [cmdBuf] 0 Nothing
    _ <- vkQueueSubmit q 1 [vkSuIn] $ VkFence nullHandle
    _ <- vkQueueWaitIdle q
    vkFreeCommandBuffers vkDev0 cmdPo0 1 [cmdBuf]
    where
        zero    = VkDeviceSize 0
        buffCo  = VkBufferCopy zero zero s

createBuffer :: VkDevice -> [VkBufferUsageFlagBits] -> VkDeviceSize -> Word32 -> IO (VkBuffer, VkDeviceMemory)
createBuffer vkDev0 f dS i = do
    vkBCI   <- createVkBufferInfo  nullPtr (VkBufferCreateFlags 0) dS f sharingModeExclusive 0 [0]
    buffer  <- vkCreateBuffer vkDev0 vkBCI
    buffMR  <- vkGetBufferMemoryRequirements vkDev0 buffer
    let vkMAI = createVkMemoryAllocateInfo nullPtr (size buffMR) i
    bufMem  <- vkAllocateMemory vkDev0 vkMAI
    _  <- vkBindBufferMemory vkDev0 buffer bufMem (VkDeviceSize 0)
    return (buffer, bufMem)

createDevice :: VkInstance -> VkSurfaceKHR -> IO VkDevice
createDevice vkInst vkSurf = do
    vkPDs   <- vkEnumeratePhysicalDevices vkInst
    let vkPD0  = head vkPDs
    vkPDF2  <- vkGetPhysicalDeviceFeatures2 vkPD0
    _       <- vkGetPhysicalDeviceSurfaceSupport vkPD0 0 vkSurf
    vkDQCI  <- createVkDeviceQueueCreateInfo nullPtr (VkDeviceQueueCreateFlags 0) 0 1 [1.0]
    vkDCI   <- createVkDeviceCreateInfo (castPtr vkPDF2) (VkDeviceCreateFlags 0) 1 vkDQCI 1 ["VK_KHR_swapchain"] Nothing
    vkCreateDevice vkPD0 vkDCI

-- Todo: Return when RayTracing is available.
createInstance :: IO VkInstance
createInstance = do
    let api = makeAPI 1 2 164
    vkApIn  <- createVkApplicationInfo nullPtr "Nightmare" 0 "Nightmare" 0 api
    vkInCI  <- createVkInstanceCreateInfo nullPtr 0 (Just vkApIn) 2 (Just ["VK_LAYER_KHRONOS_validation", "VK_LAYER_MESA_overlay"]) 3 -- 7
        (Just ["VK_EXT_debug_report", {-"VK_KHR_acceleration_structure", "VK_KHR_deferred_host_operations", "VK_KHR_pipeline_library", "VK_KHR_ray_tracing_pipeline",-} "VK_KHR_surface", "VK_KHR_xlib_surface"])
    vkCreateInstance vkInCI

createStagingBuffer :: (Num a, Storable a) => VkDevice -> [a] -> IO (VkBuffer, VkDeviceMemory)
createStagingBuffer vkDev0 x = allocaArray l $ \p -> do
    pokeArray p x
    (stage, stagMe) <- createBuffer vkDev0 [bufferUsageTransferSRCBit] s 2
    buffMa <- vkMapMemory vkDev0 stagMe (VkDeviceSize 0) s (VkMemoryMapFlags 0)
    _ <- memcpy (castPtr buffMa) p $ CSize $ 4 * w
    vkUnmapMemory vkDev0 stagMe

    return (stage, stagMe)
    where
        s = VkDeviceSize $ 4 * w
        l = length x
        w = cast l

createStagingIndexBuffer :: VkDevice -> [Word32] -> IO (VkBuffer, VkDeviceMemory)
createStagingIndexBuffer = createStagingBuffer


createStagingVertexBuffer :: VkDevice -> [Float] -> IO (VkBuffer, VkDeviceMemory)
createStagingVertexBuffer = createStagingBuffer

createSwapchain :: VkDevice -> VkSurfaceKHR -> VkFormat -> [VkImageUsageFlagBits] -> ResX -> ResY -> IO (VkSwapchainKHR, [VkImage], [VkImageView])
createSwapchain vkDev0 vkSurf format vkIUFB x y = do
    vkSCCI  <- createVkSwapchainCreateInfo nullPtr (VkSwapchainCreateFlagBitsKHR 0) vkSurf swapChainCount format
                    colorSpaceSRGBNonlinearKHR (VkExtent2D x y) 1 vkIUFB sharingModeExclusive 1 [0] surfaceTransformIdentityBitKHR
                    compositeAlphaOpaqueBitKHR {-presentModeFIFOKHR-} presentModeMailboxKHR vkTrue (VkSwapchainKHR nullHandle)
    vkSC    <- vkCreateSwapchainKHR vkDev0 vkSCCI
    swapIs  <- vkGetSwapchainImagesKHR vkDev0 vkSC
    let vkIVCI = map (\x -> VkImageViewCreateInfo structureTypeImageViewCreateInfo nullPtr (VkImageViewCreateFlags 0) x
                    imageViewType2D format vkCMId vkISR0) swapIs
    imageV   <- mapM (vkCreateImageView vkDev0) vkIVCI

    return (vkSC, swapIs, imageV)
    where
        vkISR0 = createVkImageSubresourceRange [imageAspectColorBit] 0 1 0 1
        vkCMId = VkComponentMapping componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity componentSwizzleIdentity

createSwapchainFence :: VkDevice -> IO VkFence
createSwapchainFence vkDev0 = vkCreateFence vkDev0 vkFCI
    where
        vkFCI = VkFenceCreateInfo structureTypeFenceCreateInfo nullPtr $ VkFenceCreateFlags $ unVkFenceCreateFlagBits fenceCreateSignaledBit

createSwapchainSemaphores :: VkDevice -> Int -> IO ([VkSemaphore], [VkSemaphore])
createSwapchainSemaphores vkDev0 i = do
    vkSTCI <- createVkSemaphoreTypeCreateInfo nullPtr vkSemaphoreTypeBinary 0
    vkSem0 <- replicateM i $ vkCreateSemaphore vkDev0 vkSTCI
    vkSem1 <- replicateM i $ vkCreateSemaphore vkDev0 vkSTCI
    return (vkSem0, vkSem1)

createUniformDescriptorPool :: VkDevice -> Word32 -> IO VkDescriptorPool
createUniformDescriptorPool vkDev0 c = do
    vkDPCI <- createVkDescriptorPoolCreateInfo nullPtr (VkDescriptorPoolCreateFlags 0) c 1 [vkDPS0]
    vkCreateDescriptorPool vkDev0 vkDPCI
    where
        vkDPS0 = VkDescriptorPoolSize descriptorTypeUniformBuffer c

updateUniformBuffer :: (Num a, Storable a) => VkDevice -> VkDeviceMemory -> UBO a -> IO ()
updateUniformBuffer vkDev0 uniMe ubo = allocaArray l $ \p -> do
    pokeArray p u
    buffMa <- vkMapMemory vkDev0 uniMe (VkDeviceSize 0) s (VkMemoryMapFlags 0)
    _ <- memcpy (castPtr buffMa) p $ CSize $ 4 * w
    vkUnmapMemory vkDev0 uniMe
    where
        u = uboToList ubo
        s = VkDeviceSize $ 4 * w
        l = length u
        w = cast l

initializeSwapChain :: VkDevice -> VkSurfaceKHR -> VkFormat -> [VkImageUsageFlagBits] -> ResX -> ResY -> IO ([VkFence], [VkImage], [VkImageView], ([VkSemaphore], [VkSemaphore]), VkSwapchainKHR)
initializeSwapChain vkDev0 vkSurf format vkIUFB x y = do
    (vkSC, swapIs, swapIV) <- createSwapchain vkDev0 vkSurf format vkIUFB x y
    let i = length swapIs
    semaph <- createSwapchainSemaphores vkDev0 i
    fences <- replicateM i $ createSwapchainFence vkDev0

    return (fences, swapIs, swapIV, semaph, vkSC)
{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Enumerations where


-- Vulkan newtypes
newtype VkStructureType = VkStructureType { unVkStructureType :: Int32 }
    deriving (Eq)


structureTypeApplicationInfo                                  :: VkStructureType
structureTypeApplicationInfo                                  = VkStructureType 0

{-# LANGUAGE Safe #-}

module Graphics.Vulkan.Enumerations where


import Data.Word (Word32)

import Foreign


-- Vulkan newtypes
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits { unVkBufferUsageFlagBits :: Word32 }
    deriving (Eq)
newtype VkFormat = VkFormat { unVkFormat :: Word32 }
    deriving (Eq)
newtype VkImageAspectFlagBits = VkImageAspectFlagBits { unVkImageAspectFlagBits :: Word32 }
    deriving (Eq)
newtype VkImageCreateFlagBits = VkImageCreateFlagBits { unVkImageCreateFlagBits :: Word32 }
    deriving (Eq)
newtype VkImageLayout = VkImageLayout { unVkImageLayout :: Word32 }
    deriving (Eq)
newtype VkImageTiling = VkImageTiling { unVkImageTiling :: Word32 }
    deriving (Eq)
newtype VkImageType = VkImageType { unVkImageType :: Word32 }
    deriving (Eq)
newtype VkImageUsageFlagBits = VkImageUsageFlagBits { unVkImageUsageFlagBits :: Word32 }
    deriving (Eq)
newtype VkInternalAllocationType = VkInternalAllocationType { unVkInternalAllocationType :: Word32 }
    deriving (Eq)
newtype VkResult = VkResult { unVkResult :: Int32 }
    deriving (Eq)
newtype VkSampleCountFlagBits = VkSampleCountFlagBits { unVkSampleCountFlagBits :: Word32 }
    deriving (Eq)
newtype VkShaderStageFlagBits = VkShaderStageFlagBits { unVkShaderStageFlagBits :: Word32 }
    deriving (Eq)
newtype VkSharingMode = VkSharingMode { unVkSharingMode :: Word32 }
    deriving (Eq)
newtype VkStructureType = VkStructureType { unVkStructureType :: Int32 }
    deriving (Eq)
newtype VkSystemAllocationScope = VkSystemAllocationScope { unVkSystemAllocationScope :: Word32 }
    deriving (Eq)


-- Vulkan enumerations
-- VkBufferUsageFlagBits
bufferUsageTransferSRCBit            :: VkBufferUsageFlagBits
bufferUsageTransferSRCBit            = VkBufferUsageFlagBits 1
bufferUsageTransferDSTBit            :: VkBufferUsageFlagBits
bufferUsageTransferDSTBit            = VkBufferUsageFlagBits 2
bufferUsageUniformTexelBufferBit     :: VkBufferUsageFlagBits
bufferUsageUniformTexelBufferBit     = VkBufferUsageFlagBits 4
bufferUsageStorageTexelBufferBit     :: VkBufferUsageFlagBits
bufferUsageStorageTexelBufferBit     = VkBufferUsageFlagBits 8
bufferUsageUniformBufferBit          :: VkBufferUsageFlagBits
bufferUsageUniformBufferBit          = VkBufferUsageFlagBits 16
bufferUsageStorageBufferBit          :: VkBufferUsageFlagBits
bufferUsageStorageBufferBit          = VkBufferUsageFlagBits 32
bufferUsageIndexBufferBit            :: VkBufferUsageFlagBits
bufferUsageIndexBufferBit            = VkBufferUsageFlagBits 64
bufferUsageVertexBufferBit           :: VkBufferUsageFlagBits
bufferUsageVertexBufferBit           = VkBufferUsageFlagBits 128
bufferUsageIndirectBufferBit         :: VkBufferUsageFlagBits
bufferUsageIndirectBufferBit         = VkBufferUsageFlagBits 256

-- VkFormat
-- Todo: Remove unwanted formats, like: SRGB, SFloat, UFloat
-- Todo: Remove lossy compressed formats, like: BC1-7, ETC2, EAC, ASTC
formatUndefined                  :: VkFormat
formatUndefined                  = VkFormat 0
formatR4G4UNormPack8             :: VkFormat
formatR4G4UNormPack8             = VkFormat 1
formatR4G4B4A4UNormPack16        :: VkFormat
formatR4G4B4A4UNormPack16        = VkFormat 2
formatB4G4R4A4UNormPack16        :: VkFormat
formatB4G4R4A4UNormPack16        = VkFormat 3
formatR5G6B5UNormPack16          :: VkFormat
formatR5G6B5UNormPack16          = VkFormat 4
formatB5G6R5UNormPack16          :: VkFormat
formatB5G6R5UNormPack16          = VkFormat 5
formatR5G5B5A1UNormPack16        :: VkFormat
formatR5G5B5A1UNormPack16        = VkFormat 6
formatB5G5R5A1UNormPack16        :: VkFormat
formatB5G5R5A1UNormPack16        = VkFormat 7
formatA1R5G5B5UNormPack16        :: VkFormat
formatA1R5G5B5UNormPack16        = VkFormat 8
formatR8UNorm                    :: VkFormat
formatR8UNorm                    = VkFormat 9
formatR8SNorm                    :: VkFormat
formatR8SNorm                    = VkFormat 10
formatR8UScaled                  :: VkFormat
formatR8UScaled                  = VkFormat 11
formatR8SScaled                  :: VkFormat
formatR8SScaled                  = VkFormat 12
formatR8UInt                     :: VkFormat
formatR8UInt                     = VkFormat 13
formatR8SInt                     :: VkFormat
formatR8SInt                     = VkFormat 14
formatR8SRGB                     :: VkFormat
formatR8SRGB                     = VkFormat 15
formatR8G8UNorm                  :: VkFormat
formatR8G8UNorm                  = VkFormat 16
formatR8G8SNorm                  :: VkFormat
formatR8G8SNorm                  = VkFormat 17
formatR8G8UScaled                :: VkFormat
formatR8G8UScaled                = VkFormat 18
formatR8G8SScaled                :: VkFormat
formatR8G8SScaled                = VkFormat 19
formatR8G8UInt                   :: VkFormat
formatR8G8UInt                   = VkFormat 20
formatR8G8SInt                   :: VkFormat
formatR8G8SInt                   = VkFormat 21
formatR8G8SRGB                   :: VkFormat
formatR8G8SRGB                   = VkFormat 22
formatR8G8B8UNorm                :: VkFormat
formatR8G8B8UNorm                = VkFormat 23
formatR8G8B8SNorm                :: VkFormat
formatR8G8B8SNorm                = VkFormat 24
formatR8G8B8UScaled              :: VkFormat
formatR8G8B8UScaled              = VkFormat 25
formatR8G8B8SScaled              :: VkFormat
formatR8G8B8SScaled              = VkFormat 26
formatR8G8B8UInt                 :: VkFormat
formatR8G8B8UInt                 = VkFormat 27
formatR8G8B8SInt                 :: VkFormat
formatR8G8B8SInt                 = VkFormat 28
formatR8G8B8SRGB                 :: VkFormat
formatR8G8B8SRGB                 = VkFormat 29
formatB8G8R8UNorm                :: VkFormat
formatB8G8R8UNorm                = VkFormat 30
formatB8G8R8SNorm                :: VkFormat
formatB8G8R8SNorm                = VkFormat 31
formatB8G8R8UScaled              :: VkFormat
formatB8G8R8UScaled              = VkFormat 32
formatB8G8R8SScaled              :: VkFormat
formatB8G8R8SScaled              = VkFormat 33
formatB8G8R8UInt                 :: VkFormat
formatB8G8R8UInt                 = VkFormat 34
formatB8G8R8SInt                 :: VkFormat
formatB8G8R8SInt                 = VkFormat 35
formatB8G8R8SRGB                 :: VkFormat
formatB8G8R8SRGB                 = VkFormat 36
formatR8G8B8A8UNorm              :: VkFormat
formatR8G8B8A8UNorm              = VkFormat 37
formatR8G8B8A8SNorm              :: VkFormat
formatR8G8B8A8SNorm              = VkFormat 38
formatR8G8B8A8UScaled            :: VkFormat
formatR8G8B8A8UScaled            = VkFormat 39
formatR8G8B8A8SScaled            :: VkFormat
formatR8G8B8A8SScaled            = VkFormat 40
formatR8G8B8A8UInt               :: VkFormat
formatR8G8B8A8UInt               = VkFormat 41
formatR8G8B8A8SInt               :: VkFormat
formatR8G8B8A8SInt               = VkFormat 42
formatR8G8B8A8SRGB               :: VkFormat
formatR8G8B8A8SRGB               = VkFormat 43
formatB8G8R8A8UNorm              :: VkFormat
formatB8G8R8A8UNorm              = VkFormat 44
formatB8G8R8A8SNorm              :: VkFormat
formatB8G8R8A8SNorm              = VkFormat 45
formatB8G8R8A8UScaled            :: VkFormat
formatB8G8R8A8UScaled            = VkFormat 46
formatB8G8R8A8SScaled            :: VkFormat
formatB8G8R8A8SScaled            = VkFormat 47
formatB8G8R8A8UInt               :: VkFormat
formatB8G8R8A8UInt               = VkFormat 48
formatB8G8R8A8SInt               :: VkFormat
formatB8G8R8A8SInt               = VkFormat 49
formatB8G8R8A8SRGB               :: VkFormat
formatB8G8R8A8SRGB               = VkFormat 50
formatA8B8G8R8UNormPack32        :: VkFormat
formatA8B8G8R8UNormPack32        = VkFormat 51
formatA8B8G8R8SNormPack32        :: VkFormat
formatA8B8G8R8SNormPack32        = VkFormat 52
formatA8B8G8R8UScaledPack32      :: VkFormat
formatA8B8G8R8UScaledPack32      = VkFormat 53
formatA8B8G8R8SScaledPack32      :: VkFormat
formatA8B8G8R8SScaledPack32      = VkFormat 54
formatA8B8G8R8UIntPack32         :: VkFormat
formatA8B8G8R8UIntPack32         = VkFormat 55
formatA8B8G8R8SIntPack32         :: VkFormat
formatA8B8G8R8SIntPack32         = VkFormat 56
formatA8B8G8R8SRGBPack32         :: VkFormat
formatA8B8G8R8SRGBPack32         = VkFormat 57
formatA2R10G10B10UNormPack32     :: VkFormat
formatA2R10G10B10UNormPack32     = VkFormat 58
formatA2R10G10B10SNormPack32     :: VkFormat
formatA2R10G10B10SNormPack32     = VkFormat 59
formatA2R10G10B10UScaledPack32   :: VkFormat
formatA2R10G10B10UScaledPack32   = VkFormat 60
formatA2R10G10B10SScaledPack32   :: VkFormat
formatA2R10G10B10SScaledPack32   = VkFormat 61
formatA2R10G10B10UIntPack32      :: VkFormat
formatA2R10G10B10UIntPack32      = VkFormat 62
formatA2R10G10B10SIntPack32      :: VkFormat
formatA2R10G10B10SIntPack32      = VkFormat 63
formatA2B10G10R10UNormPack32     :: VkFormat
formatA2B10G10R10UNormPack32     = VkFormat 64
formatA2B10G10R10SNormPack32     :: VkFormat
formatA2B10G10R10SNormPack32     = VkFormat 65
formatA2B10G10R10UScaledPack32   :: VkFormat
formatA2B10G10R10UScaledPack32   = VkFormat 66
formatA2B10G10R10SScaledPack32   :: VkFormat
formatA2B10G10R10SScaledPack32   = VkFormat 67
formatA2B10G10R10UIntPack32      :: VkFormat
formatA2B10G10R10UIntPack32      = VkFormat 68
formatA2B10G10R10SIntPack32      :: VkFormat
formatA2B10G10R10SIntPack32      = VkFormat 69
formatR16UNorm                   :: VkFormat
formatR16UNorm                   = VkFormat 70
formatR16SNorm                   :: VkFormat
formatR16SNorm                   = VkFormat 71
formatR16UScaled                 :: VkFormat
formatR16UScaled                 = VkFormat 72
formatR16SScaled                 :: VkFormat
formatR16SScaled                 = VkFormat 73
formatR16UInt                    :: VkFormat
formatR16UInt                    = VkFormat 74
formatR16SInt                    :: VkFormat
formatR16SInt                    = VkFormat 75
formatR16SFloat                  :: VkFormat
formatR16SFloat                  = VkFormat 76
formatR16G16UNorm                :: VkFormat
formatR16G16UNorm                = VkFormat 77
formatR16G16SNorm                :: VkFormat
formatR16G16SNorm                = VkFormat 78
formatR16G16Scaled               :: VkFormat
formatR16G16Scaled               = VkFormat 79
formatR16G16SScaled              :: VkFormat
formatR16G16SScaled              = VkFormat 80
formatR16G16UInt                 :: VkFormat
formatR16G16UInt                 = VkFormat 81
formatR16G16SInt                 :: VkFormat
formatR16G16SInt                 = VkFormat 82
formatR16G16SFloat               :: VkFormat
formatR16G16SFloat               = VkFormat 83
formatR16G16B16UNorm             :: VkFormat
formatR16G16B16UNorm             = VkFormat 84
formatR16G16B16SNorm             :: VkFormat
formatR16G16B16SNorm             = VkFormat 85
formatR16G16B16UScaled           :: VkFormat
formatR16G16B16UScaled           = VkFormat 86
formatR16G16B16SScaled           :: VkFormat
formatR16G16B16SScaled           = VkFormat 87
formatR16G16B16UInt              :: VkFormat
formatR16G16B16UInt              = VkFormat 88
formatR16G16B16SInt              :: VkFormat
formatR16G16B16SInt              = VkFormat 89
formatR16G16B16SFloat            :: VkFormat
formatR16G16B16SFloat            = VkFormat 90
formatR16G16B16A16UNorm          :: VkFormat
formatR16G16B16A16UNorm          = VkFormat 91
formatR16G16B16A16SNorm          :: VkFormat
formatR16G16B16A16SNorm          = VkFormat 92
formatR16G16B16A16UScaled        :: VkFormat
formatR16G16B16A16UScaled        = VkFormat 93
formatR16G16B16A16SScaled        :: VkFormat
formatR16G16B16A16SScaled        = VkFormat 94
formatR16G16B16A16UInt           :: VkFormat
formatR16G16B16A16UInt           = VkFormat 95
formatR16G16B16A16SInt           :: VkFormat
formatR16G16B16A16SInt           = VkFormat 96
formatR16G16B16A16SFloat         :: VkFormat
formatR16G16B16A16SFloat         = VkFormat 97
formatR32UInt                    :: VkFormat
formatR32UInt                    = VkFormat 98
formatR32SInt                    :: VkFormat
formatR32SInt                    = VkFormat 99
formatR32SFloat                  :: VkFormat
formatR32SFloat                  = VkFormat 100
formatR32G32UInt                 :: VkFormat
formatR32G32UInt                 = VkFormat 101
formatR32G32SInt                 :: VkFormat
formatR32G32SInt                 = VkFormat 102
formatR32G32SFloat               :: VkFormat
formatR32G32SFloat               = VkFormat 103
formatR32G32B32UInt              :: VkFormat
formatR32G32B32UInt              = VkFormat 104
formatR32G32B32SInt              :: VkFormat
formatR32G32B32SInt              = VkFormat 105
formatR32G32B32SFloat            :: VkFormat
formatR32G32B32SFloat            = VkFormat 106
formatR32G32B32A32UInt           :: VkFormat
formatR32G32B32A32UInt           = VkFormat 107
formatR32G32B32A32SInt           :: VkFormat
formatR32G32B32A32SInt           = VkFormat 108
formatR32G32B32A32SFloat         :: VkFormat
formatR32G32B32A32SFloat         = VkFormat 109
formatR64UInt                    :: VkFormat
formatR64UInt                    = VkFormat 110
formatR64SInt                    :: VkFormat
formatR64SInt                    = VkFormat 111
formatR64SFloat                  :: VkFormat
formatR64SFloat                  = VkFormat 112
formatR64G64UInt                 :: VkFormat
formatR64G64UInt                 = VkFormat 113
formatR64G64SInt                 :: VkFormat
formatR64G64SInt                 = VkFormat 114
formatR64G64SFloat               :: VkFormat
formatR64G64SFloat               = VkFormat 115
formatR64G64B64UInt              :: VkFormat
formatR64G64B64UInt              = VkFormat 116
formatR64G64B64SInt              :: VkFormat
formatR64G64B64SInt              = VkFormat 117
formatR64G64B64SFloat            :: VkFormat
formatR64G64B64SFloat            = VkFormat 118
formatR64G64B64A64UInt           :: VkFormat
formatR64G64B64A64UInt           = VkFormat 119
formatR64G64B64A64SInt           :: VkFormat
formatR64G64B64A64SInt           = VkFormat 120
formatR64G64B64A64SFloat         :: VkFormat
formatR64G64B64A64SFloat         = VkFormat 121
formatB10G11R11UFloatPack32      :: VkFormat
formatB10G11R11UFloatPack32      = VkFormat 122
formatE5B9G9R9UFloatPack32       :: VkFormat
formatE5B9G9R9UFloatPack32       = VkFormat 123
formatD16UNorm                   :: VkFormat
formatD16UNorm                   = VkFormat 124
formatX8D24UNormPack32           :: VkFormat
formatX8D24UNormPack32           = VkFormat 125
formatD32SFloat                  :: VkFormat
formatD32SFloat                  = VkFormat 126
formatS8UInt                     :: VkFormat
formatS8UInt                     = VkFormat 127
formatD16UNormS8UInt             :: VkFormat
formatD16UNormS8UInt             = VkFormat 128
formatD24UNormS8UInt             :: VkFormat
formatD24UNormS8UInt             = VkFormat 129
formatD32SFloatS8UInt            :: VkFormat
formatD32SFloatS8UInt            = VkFormat 130
formatBC1RGBUNormBlock           :: VkFormat
formatBC1RGBUNormBlock           = VkFormat 131
formatBC1RGBSRGBBlock            :: VkFormat
formatBC1RGBSRGBBlock            = VkFormat 132
formatBC1RGBAUNormBlock          :: VkFormat
formatBC1RGBAUNormBlock          = VkFormat 133
formatBC1RGBASRGBBlock           :: VkFormat
formatBC1RGBASRGBBlock           = VkFormat 134
formatBC2UNormBlock              :: VkFormat
formatBC2UNormBlock              = VkFormat 135
formatBC2SRGBBlock               :: VkFormat
formatBC2SRGBBlock               = VkFormat 136
formatBC3UNormBlock              :: VkFormat
formatBC3UNormBlock              = VkFormat 137
formatBC3SRGBBlock               :: VkFormat
formatBC3SRGBBlock               = VkFormat 138
formatBC4UNorm                   :: VkFormat
formatBC4UNorm                   = VkFormat 139
formatBC4SNorm                   :: VkFormat
formatBC4SNorm                   = VkFormat 140
formatBC5UNormBlock              :: VkFormat
formatBC5UNormBlock              = VkFormat 141
formatBC5SNormBlock              :: VkFormat
formatBC5SNormBlock              = VkFormat 142
formatBC6HUFloatBlock            :: VkFormat
formatBC6HUFloatBlock            = VkFormat 143
formatBC6HSFloatBlock            :: VkFormat
formatBC6HSFloatBlock            = VkFormat 144
formatBC7UNormBlock              :: VkFormat
formatBC7UNormBlock              = VkFormat 145
formatBC7SRGBBlock               :: VkFormat
formatBC7SRGBBlock               = VkFormat 146
formatETC2R8G8B8UNormBlock       :: VkFormat
formatETC2R8G8B8UNormBlock       = VkFormat 147
formatETC2R8G8B8SRGBBlock        :: VkFormat
formatETC2R8G8B8SRGBBlock        = VkFormat 148
formatETC2R8G8B8A1UNormBlock     :: VkFormat
formatETC2R8G8B8A1UNormBlock     = VkFormat 149
formatETC2R8G8B8A1SRGBBlock      :: VkFormat
formatETC2R8G8B8A1SRGBBlock      = VkFormat 150
formatETC2R8G8B8A8UNormBlock     :: VkFormat
formatETC2R8G8B8A8UNormBlock     = VkFormat 151
formatETC2R8G8B8A8SRGBBlock      :: VkFormat
formatETC2R8G8B8A8SRGBBlock      = VkFormat 152
formatEACR11UNormBlock           :: VkFormat
formatEACR11UNormBlock           = VkFormat 153
formatEACR11SNormBlock           :: VkFormat
formatEACR11SNormBlock           = VkFormat 154
formatEACR11G11UNormBlock        :: VkFormat
formatEACR11G11UNormBlock        = VkFormat 155
formatEACR11G11SNormBlock        :: VkFormat
formatEACR11G11SNormBlock        = VkFormat 156
formatASTC4x4UNormBlock          :: VkFormat
formatASTC4x4UNormBlock          = VkFormat 157
formatASTC4x4SRGBBlock           :: VkFormat
formatASTC4x4SRGBBlock           = VkFormat 158
formatASTC5x4UNormBlock          :: VkFormat
formatASTC5x4UNormBlock          = VkFormat 159
formatASTC5x4SRGBBlock           :: VkFormat
formatASTC5x4SRGBBlock           = VkFormat 160
formatASTC5x5UNormBlock          :: VkFormat
formatASTC5x5UNormBlock          = VkFormat 161
formatASTC5x5SRGBBlock           :: VkFormat
formatASTC5x5SRGBBlock           = VkFormat 162
formatASTC6x5UNormBlock          :: VkFormat
formatASTC6x5UNormBlock          = VkFormat 163
formatASTC6x5SRGBBlock           :: VkFormat
formatASTC6x5SRGBBlock           = VkFormat 164
formatASTC6x6UNormBlock          :: VkFormat
formatASTC6x6UNormBlock          = VkFormat 165
formatASTC6x6SRGBBlock           :: VkFormat
formatASTC6x6SRGBBlock           = VkFormat 166
formatASTC8x5UNormBlock          :: VkFormat
formatASTC8x5UNormBlock          = VkFormat 167
formatASTC8x5SRGBBlock           :: VkFormat
formatASTC8x5SRGBBlock           = VkFormat 168
formatASTC8x6UNormBlock          :: VkFormat
formatASTC8x6UNormBlock          = VkFormat 169
formatASTC8x6SRGBBlock           :: VkFormat
formatASTC8x6SRGBBlock           = VkFormat 170
formatASTC8x8UNormBlock          :: VkFormat
formatASTC8x8UNormBlock          = VkFormat 171
formatASTC8x8SRGBBlock           :: VkFormat
formatASTC8x8SRGBBlock           = VkFormat 172
formatASTC10x5UNormBlock         :: VkFormat
formatASTC10x5UNormBlock         = VkFormat 173
formatASTC10x5SRGBBlock          :: VkFormat
formatASTC10x5SRGBBlock          = VkFormat 174
formatASTC10x6UNormBlock         :: VkFormat
formatASTC10x6UNormBlock         = VkFormat 175
formatASTC10x6SRGBBlock          :: VkFormat
formatASTC10x6SRGBBlock          = VkFormat 176
formatASTC10x8UNormBlock         :: VkFormat
formatASTC10x8UNormBlock         = VkFormat 177
formatASTC10x8SRGBBlock          :: VkFormat
formatASTC10x8SRGBBlock          = VkFormat 178
formatASTC10x10UNormBlock        :: VkFormat
formatASTC10x10UNormBlock        = VkFormat 179
formatASTC10x10SRGBBlock         :: VkFormat
formatASTC10x10SRGBBlock         = VkFormat 180
formatASTC12x10UNormBlock        :: VkFormat
formatASTC12x10UNormBlock        = VkFormat 181
formatASTC12x10SRGBBlock         :: VkFormat
formatASTC12x10SRGBBlock         = VkFormat 182
formatASTC12x12UNormBlock        :: VkFormat
formatASTC12x12UNormBlock        = VkFormat 183
formatASTC12x12SRGBBlock         :: VkFormat
formatASTC12x12SRGBBlock         = VkFormat 184
formatPVRTC12BPPUNormBlockImg    :: VkFormat
formatPVRTC12BPPUNormBlockImg    = VkFormat 1000054000
formatPVRTC14BPPUNormBlockImg    :: VkFormat
formatPVRTC14BPPUNormBlockImg    = VkFormat 1000054001
formatPVRTC22BPPUNormBlockImg    :: VkFormat
formatPVRTC22BPPUNormBlockImg    = VkFormat 1000054002
formatPVRTC24BPPUNormBlockImg    :: VkFormat
formatPVRTC24BPPUNormBlockImg    = VkFormat 1000054003
formatPVRTC12BPPSRGBBlockImg     :: VkFormat
formatPVRTC12BPPSRGBBlockImg     = VkFormat 1000054004
formatPVRTC14BPPSRGBBlockImg     :: VkFormat
formatPVRTC14BPPSRGBBlockImg     = VkFormat 1000054005
formatPVRTC22BPPSRGBBlockImg     :: VkFormat
formatPVRTC22BPPSRGBBlockImg     = VkFormat 1000054006
formatPVRTC24BPPSRGBBlockImg     :: VkFormat
formatPVRTC24BPPSRGBBlockImg     = VkFormat 1000054007

-- VkImageAspectFlagBits
imageAspectColorBit          :: VkImageAspectFlagBits
imageAspectColorBit          = VkImageAspectFlagBits 1
imageAspectDepthBit          :: VkImageAspectFlagBits
imageAspectDepthBit          = VkImageAspectFlagBits 2
imageAspectStencilBit        :: VkImageAspectFlagBits
imageAspectStencilBit        = VkImageAspectFlagBits 4
imageAspectMetadataBit       :: VkImageAspectFlagBits
imageAspectMetadataBit       = VkImageAspectFlagBits 8

-- VkImageCreateFlagBits
imageCreateSparseBindingBit      :: VkImageCreateFlagBits
imageCreateSparseBindingBit      = VkImageCreateFlagBits 1
imageCreateSparseResidencyBit    :: VkImageCreateFlagBits
imageCreateSparseResidencyBit    = VkImageCreateFlagBits 2
imageCreateSparseAliasedBit      :: VkImageCreateFlagBits
imageCreateSparseAliasedBit      = VkImageCreateFlagBits 4
imageCreateMutableFormatBit      :: VkImageCreateFlagBits
imageCreateMutableFormatBit      = VkImageCreateFlagBits 8
imageCreateCubeCompatibleBit     :: VkImageCreateFlagBits
imageCreateCubeCompatibleBit     = VkImageCreateFlagBits 16

-- VkImageLayout
imageLayoutUndefined                         :: VkImageLayout
imageLayoutUndefined                         = VkImageLayout 0
imageLayoutGeneral                           :: VkImageLayout
imageLayoutGeneral                           = VkImageLayout 1
imageLayoutColorAttachmentOptimal            :: VkImageLayout
imageLayoutColorAttachmentOptimal            = VkImageLayout 2
imageLayoutDepthStencilAttachmentOptimal     :: VkImageLayout
imageLayoutDepthStencilAttachmentOptimal     = VkImageLayout 3
imageLayoutDepthStencilReadOnlyOptimal       :: VkImageLayout
imageLayoutDepthStencilReadOnlyOptimal       = VkImageLayout 4
imageLayoutShaderReadOnlyOptimal             :: VkImageLayout
imageLayoutShaderReadOnlyOptimal             = VkImageLayout 5
imageLayoutTransferSRCOptimal                :: VkImageLayout
imageLayoutTransferSRCOptimal                = VkImageLayout 6
imageLayoutTransferDSTOptimal                :: VkImageLayout
imageLayoutTransferDSTOptimal                = VkImageLayout 7
imageLayoutPreinitialized                    :: VkImageLayout
imageLayoutPreinitialized                    = VkImageLayout 8
imageLayoutPresentSRCKHR                     :: VkImageLayout
imageLayoutPresentSRCKHR                     = VkImageLayout 1000001002

-- VkImageTiling
imageTilingOptimal       :: VkImageTiling
imageTilingOptimal       = VkImageTiling 0
imageTilingLinear        :: VkImageTiling
imageTilingLinear        = VkImageTiling 1

-- VkImageType
imageType1D          :: VkImageType
imageType1D          = VkImageType 0
imageType2D          :: VkImageType
imageType2D          = VkImageType 1
imageType3D          :: VkImageType
imageType3D          = VkImageType 2

-- VkImageUsageFlagBits
imageUsageTransferSRCBit             :: VkImageUsageFlagBits
imageUsageTransferSRCBit             = VkImageUsageFlagBits 1
imageUsageTransferDSTBit             :: VkImageUsageFlagBits
imageUsageTransferDSTBit             = VkImageUsageFlagBits 2
imageUsageSampledBit                 :: VkImageUsageFlagBits
imageUsageSampledBit                 = VkImageUsageFlagBits 4
imageUsageStorageBit                 :: VkImageUsageFlagBits
imageUsageStorageBit                 = VkImageUsageFlagBits 8
imageUsageColorAttachmentBit         :: VkImageUsageFlagBits
imageUsageColorAttachmentBit         = VkImageUsageFlagBits 16
imageUsageDepthStencilAttachmentBit  :: VkImageUsageFlagBits
imageUsageDepthStencilAttachmentBit  = VkImageUsageFlagBits 32
imageUsageTransientAttachmentBit     :: VkImageUsageFlagBits
imageUsageTransientAttachmentBit     = VkImageUsageFlagBits 64
imageUsageInputAttachmentBit         :: VkImageUsageFlagBits
imageUsageInputAttachmentBit         = VkImageUsageFlagBits 128

-- VkInternalAllocationType
internalAllocationTypeExecutable     :: VkInternalAllocationType
internalAllocationTypeExecutable     = VkInternalAllocationType 0

-- VkResult
success                      :: VkResult
success                      = VkResult 0
notReady                     :: VkResult
notReady                     = VkResult 1
timeout                      :: VkResult
timeout                      = VkResult 2
eventSet                     :: VkResult
eventSet                     = VkResult 3
eventReset                   :: VkResult
eventReset                   = VkResult 4
incomplete                   :: VkResult
incomplete                   = VkResult 5
errorOutOfHostMemory         :: VkResult
errorOutOfHostMemory         = VkResult (-1)
errorOutOfDeviceMemory       :: VkResult
errorOutOfDeviceMemory       = VkResult (-2)
errorInitializationFailed    :: VkResult
errorInitializationFailed    = VkResult (-3)
errorDeviceLost              :: VkResult
errorDeviceLost              = VkResult (-4)
errorMemoryFailed            :: VkResult
errorMemoryFailed            = VkResult (-5)
errorLayerNotPresent         :: VkResult
errorLayerNotPresent         = VkResult (-6)
errorExtensionNotPresent     :: VkResult
errorExtensionNotPresent     = VkResult (-7)
errorFeatureNotPresent       :: VkResult
errorFeatureNotPresent       = VkResult (-8)
errorIncompatibleDriver      :: VkResult
errorIncompatibleDriver      = VkResult (-9)
errorTooManyObjects          :: VkResult
errorTooManyObjects          = VkResult (-10)
errorFormatNotSupported      :: VkResult
errorFormatNotSupported      = VkResult (-11)
errorFragmentedPool          :: VkResult
errorFragmentedPool          = VkResult (-12)
errorSurfaceLostKHR          :: VkResult
errorSurfaceLostKHR          = VkResult (-1000000000)
errorNativeWindowInUseKHR    :: VkResult
errorNativeWindowInUseKHR    = VkResult (-1000000001)
suboptimalKHR                :: VkResult
suboptimalKHR                = VkResult 1000001003
errorOutOfDateKHR            :: VkResult
errorOutOfDateKHR            = VkResult (-1000001004)
errorIncompatibleDisplayKHR  :: VkResult
errorIncompatibleDisplayKHR  = VkResult (-1000003001)
errorValidationFailedEXT     :: VkResult
errorValidationFailedEXT     = VkResult (-1000011001)
errorInvalidShaderNV         :: VkResult
errorInvalidShaderNV         = VkResult (-1000012000)

-- VkSampleCountFlagBits
sampleCount1Bit              :: VkSampleCountFlagBits
sampleCount1Bit              = VkSampleCountFlagBits 1
sampleCount2Bit              :: VkSampleCountFlagBits
sampleCount2Bit              = VkSampleCountFlagBits 2
sampleCount4Bit              :: VkSampleCountFlagBits
sampleCount4Bit              = VkSampleCountFlagBits 4
sampleCount8Bit              :: VkSampleCountFlagBits
sampleCount8Bit              = VkSampleCountFlagBits 8
sampleCount16Bit             :: VkSampleCountFlagBits
sampleCount16Bit             = VkSampleCountFlagBits 16
sampleCount32Bit             :: VkSampleCountFlagBits
sampleCount32Bit             = VkSampleCountFlagBits 32
sampleCount64Bit             :: VkSampleCountFlagBits
sampleCount64Bit             = VkSampleCountFlagBits 64

-- VkShaderStageFlagBits
shaderStageVertexBit                     :: VkShaderStageFlagBits
shaderStageVertexBit                     = VkShaderStageFlagBits 1
shaderStageTessellationControlBit        :: VkShaderStageFlagBits
shaderStageTessellationControlBit        = VkShaderStageFlagBits 2
shaderStageTessellationEvaluationBit     :: VkShaderStageFlagBits
shaderStageTessellationEvaluationBit     = VkShaderStageFlagBits 4
shaderStageGeometryBit                   :: VkShaderStageFlagBits
shaderStageGeometryBit                   = VkShaderStageFlagBits 8
shaderStageFragmentBit                   :: VkShaderStageFlagBits
shaderStageFragmentBit                   = VkShaderStageFlagBits 16
shaderStageComputeBit                    :: VkShaderStageFlagBits
shaderStageComputeBit                    = VkShaderStageFlagBits 32
shaderStageAllGraphics                   :: VkShaderStageFlagBits
shaderStageAllGraphics                   = VkShaderStageFlagBits 31
shaderStageAll                           :: VkShaderStageFlagBits
shaderStageAll                           = VkShaderStageFlagBits 2147483647

-- VkSharingMode
sharingModeExclusive     :: VkSharingMode
sharingModeExclusive     = VkSharingMode 0
sharingModeConcurrent    :: VkSharingMode
sharingModeConcurrent    = VkSharingMode 1

-- VkStructureType
structureTypeApplicationInfo                                  :: VkStructureType
structureTypeApplicationInfo                                  = VkStructureType 0
structureTypeInstanceCreateInfo                               :: VkStructureType
structureTypeInstanceCreateInfo                               = VkStructureType 1
structureTypeDeviceQueueCreateInfo                            :: VkStructureType
structureTypeDeviceQueueCreateInfo                            = VkStructureType 2
structureTypeDeviceCreateInfo                                 :: VkStructureType
structureTypeDeviceCreateInfo                                 = VkStructureType 3
structureTypeSubmitInfo                                       :: VkStructureType
structureTypeSubmitInfo                                       = VkStructureType 4
structureTypeMemoryAllocateInfo                               :: VkStructureType
structureTypeMemoryAllocateInfo                               = VkStructureType 5
structureTypeMappedMemoryRange                                :: VkStructureType
structureTypeMappedMemoryRange                                = VkStructureType 6
structureTypeBindSparseInfo                                   :: VkStructureType
structureTypeBindSparseInfo                                   = VkStructureType 7
structureTypeFenceCreateInfo                                  :: VkStructureType
structureTypeFenceCreateInfo                                  = VkStructureType 8
structureTypeSemaphreCreateInfo                               :: VkStructureType
structureTypeSemaphreCreateInfo                               = VkStructureType 9
structureTypeEventCreateInfo                                  :: VkStructureType
structureTypeEventCreateInfo                                  = VkStructureType 10
structureTypeQueryPoolCreateInfo                              :: VkStructureType
structureTypeQueryPoolCreateInfo                              = VkStructureType 11
structureTypeBufferCreateInfo                                 :: VkStructureType
structureTypeBufferCreateInfo                                 = VkStructureType 12
structureTypeBufferViewCreateInfo                             :: VkStructureType
structureTypeBufferViewCreateInfo                             = VkStructureType 13
structureTypeImageCreateInfo                                  :: VkStructureType
structureTypeImageCreateInfo                                  = VkStructureType 14
structureTypeImageViewCreateInfo                              :: VkStructureType
structureTypeImageViewCreateInfo                              = VkStructureType 15
structureTypeShaderModuleCreateInfo                           :: VkStructureType
structureTypeShaderModuleCreateInfo                           = VkStructureType 16
structureTypePipelineCacheCreateInfo                          :: VkStructureType
structureTypePipelineCacheCreateInfo                          = VkStructureType 17
structureTypePipelineShaderStageCreateInfo                    :: VkStructureType
structureTypePipelineShaderStageCreateInfo                    = VkStructureType 18
structureTypePipelineVertexInputStateCreateInfo               :: VkStructureType
structureTypePipelineVertexInputStateCreateInfo               = VkStructureType 19
structureTypePipelineInputAssembyStateCreateInfo              :: VkStructureType
structureTypePipelineInputAssembyStateCreateInfo              = VkStructureType 20
structureTypePipelineTessellationStateCreateInfo              :: VkStructureType
structureTypePipelineTessellationStateCreateInfo              = VkStructureType 21
structureTypePipelineViewportStateCreateInfo                  :: VkStructureType
structureTypePipelineViewportStateCreateInfo                  = VkStructureType 22
structureTypePipelineRaterizationStateCreateInfo              :: VkStructureType
structureTypePipelineRaterizationStateCreateInfo              = VkStructureType 23
structureTypePipelineMultisampleStateCreateInfo               :: VkStructureType
structureTypePipelineMultisampleStateCreateInfo               = VkStructureType 24
structureTypePipelineDepthStencilStateCreateInfo              :: VkStructureType
structureTypePipelineDepthStencilStateCreateInfo              = VkStructureType 25
structureTypePipelineColorBlendStateCreateInfo                :: VkStructureType
structureTypePipelineColorBlendStateCreateInfo                = VkStructureType 26
structureTypePipelineDynamicStateCreateInfo                   :: VkStructureType
structureTypePipelineDynamicStateCreateInfo                   = VkStructureType 27
structureTypeGraphicsPipelineCreateInfo                       :: VkStructureType
structureTypeGraphicsPipelineCreateInfo                       = VkStructureType 28
structureTypeComputePipelineCreateInfo                        :: VkStructureType
structureTypeComputePipelineCreateInfo                        = VkStructureType 29
structureTypePipelineLayoutCreateInfo                         :: VkStructureType
structureTypePipelineLayoutCreateInfo                         = VkStructureType 30
structureTypeSamplerCreateInfo                                :: VkStructureType
structureTypeSamplerCreateInfo                                = VkStructureType 31
structureTypeDescriptorSetLayoutCreateInfo                    :: VkStructureType
structureTypeDescriptorSetLayoutCreateInfo                    = VkStructureType 32
structureTypeDescriptorPoolCreateInfo                         :: VkStructureType
structureTypeDescriptorPoolCreateInfo                         = VkStructureType 33
structureTypeDescriptorSetAllocateInfo                        :: VkStructureType
structureTypeDescriptorSetAllocateInfo                        = VkStructureType 34
structureTypeWriteDescriptorSet                               :: VkStructureType
structureTypeWriteDescriptorSet                               = VkStructureType 35
structureTypeCopyDescriptorSet                                :: VkStructureType
structureTypeCopyDescriptorSet                                = VkStructureType 36
structureTypeFramebufferCreateInfo                            :: VkStructureType
structureTypeFramebufferCreateInfo                            = VkStructureType 37
structureTypeRenderPassCreateInfo                             :: VkStructureType
structureTypeRenderPassCreateInfo                             = VkStructureType 38
structureTypeCommandPoolCreateInfo                            :: VkStructureType
structureTypeCommandPoolCreateInfo                            = VkStructureType 39
structureTypeCommandBufferAllocateInfo                        :: VkStructureType
structureTypeCommandBufferAllocateInfo                        = VkStructureType 40
structureTypeCommandBufferInheritanceInfo                     :: VkStructureType
structureTypeCommandBufferInheritanceInfo                     = VkStructureType 41
structureTypeCommandBufferBeginInfo                           :: VkStructureType
structureTypeCommandBufferBeginInfo                           = VkStructureType 42
structureTypeRenderPassBeginInfo                              :: VkStructureType
structureTypeRenderPassBeginInfo                              = VkStructureType 43
structureTypeBufferMemoryBarrier                              :: VkStructureType
structureTypeBufferMemoryBarrier                              = VkStructureType 44
structureTypeImageMemoryBarrier                               :: VkStructureType
structureTypeImageMemoryBarrier                               = VkStructureType 45
structureTypeMemoryBarrier                                    :: VkStructureType
structureTypeMemoryBarrier                                    = VkStructureType 46
structureTypeLoaderInstanceCreateInfo                         :: VkStructureType
structureTypeLoaderInstanceCreateInfo                         = VkStructureType 47
structureTypeLoaderDeviceCreateInfo                           :: VkStructureType
structureTypeLoaderDeviceCreateInfo                           = VkStructureType 48
structureTypeSwapchainCreateInfoKHR                           :: VkStructureType
structureTypeSwapchainCreateInfoKHR                           = VkStructureType 1000001000
structureTypePresentInfoKHR                                   :: VkStructureType
structureTypePresentInfoKHR                                   = VkStructureType 1000001001
structureTypeDisplayModeCreateInfoKHR                         :: VkStructureType
structureTypeDisplayModeCreateInfoKHR                         = VkStructureType 1000002000
structureTypeDisplaySurfaceCreateInfoKHR                      :: VkStructureType
structureTypeDisplaySurfaceCreateInfoKHR                      = VkStructureType 1000002001
structureTypeDisplayPresentInfoKHR                            :: VkStructureType
structureTypeDisplayPresentInfoKHR                            = VkStructureType 1000003000
structureTypeDebugReportCallbackCreateInfoEXT                 :: VkStructureType
structureTypeDebugReportCallbackCreateInfoEXT                 = VkStructureType 1000011000
structureTypePipelineRasterizationStateRasterizationOrderAMD  :: VkStructureType
structureTypePipelineRasterizationStateRasterizationOrderAMD  = VkStructureType 1000018000
structureTypeDebugMarkerObjectNameInfoEXT                     :: VkStructureType
structureTypeDebugMarkerObjectNameInfoEXT                     = VkStructureType 1000022000
structureTypeDebugMarkerObjectTagInfoEXT                      :: VkStructureType
structureTypeDebugMarkerObjectTagInfoEXT                      = VkStructureType 1000022001
structureTypeDebugMarkerMarkerInfoEXT                         :: VkStructureType
structureTypeDebugMarkerMarkerInfoEXT                         = VkStructureType 1000022002
structureTypeExternalMemoryImageCreateInfoNV                  :: VkStructureType
structureTypeExternalMemoryImageCreateInfoNV                  = VkStructureType 1000056000
structureTypeExportMemoryAllocateInfoNV                       :: VkStructureType
structureTypeExportMemoryAllocateInfoNV                       = VkStructureType 1000056001
structureTypeValidationFlagsEXT                               :: VkStructureType
structureTypeValidationFlagsEXT                               = VkStructureType 1000061000

-- VkSystemAllocationScope
systemAllocationScopeCommand     :: VkSystemAllocationScope
systemAllocationScopeCommand     = VkSystemAllocationScope 0
systemAllocationScopeObject      :: VkSystemAllocationScope
systemAllocationScopeObject      = VkSystemAllocationScope 1
systemAllocationScopeCache       :: VkSystemAllocationScope
systemAllocationScopeCache       = VkSystemAllocationScope 2
systemAllocationScopeDevice      :: VkSystemAllocationScope
systemAllocationScopeDevice      = VkSystemAllocationScope 3
systemAllocationScopeInstance    :: VkSystemAllocationScope
systemAllocationScopeInstance    = VkSystemAllocationScope 4


instance Storable VkBufferUsageFlagBits where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkBufferUsageFlagBits v)
    poke p (VkBufferUsageFlagBits v) = pokeByteOff p 0 v

instance Storable VkFormat where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkFormat v)
    poke p (VkFormat v) = pokeByteOff p 0 v

instance Storable VkImageAspectFlagBits where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageAspectFlagBits v)
    poke p (VkImageAspectFlagBits v) = pokeByteOff p 0 v

instance Storable VkImageCreateFlagBits where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageCreateFlagBits v)
    poke p (VkImageCreateFlagBits v) = pokeByteOff p 0 v

instance Storable VkImageLayout where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageLayout v)
    poke p (VkImageLayout v) = pokeByteOff p 0 v

instance Storable VkImageTiling where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageTiling v)
    poke p (VkImageTiling v) = pokeByteOff p 0 v

instance Storable VkImageType where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageType v)
    poke p (VkImageType v) = pokeByteOff p 0 v

instance Storable VkImageUsageFlagBits where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkImageUsageFlagBits v)
    poke p (VkImageUsageFlagBits v) = pokeByteOff p 0 v

instance Storable VkInternalAllocationType where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkInternalAllocationType v)
    poke p (VkInternalAllocationType v) = pokeByteOff p 0 v

instance Storable VkResult where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkResult v)
    poke p (VkResult v) = pokeByteOff p 0 v

instance Storable VkSampleCountFlagBits where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSampleCountFlagBits v)
    poke p (VkSampleCountFlagBits v) = pokeByteOff p 0 v

instance Storable VkShaderStageFlagBits where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkShaderStageFlagBits v)
    poke p (VkShaderStageFlagBits v) = pokeByteOff p 0 v

instance Storable VkSharingMode where
    sizeOf _ = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSharingMode v)
    poke p (VkSharingMode v) = pokeByteOff p 0 v

instance Storable VkStructureType where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkStructureType v)
    poke p (VkStructureType v) = pokeByteOff p 0 v

instance Storable VkSystemAllocationScope where
    sizeOf _    = 4
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (VkSystemAllocationScope v)
    poke p (VkSystemAllocationScope v) = pokeByteOff p 0 v

{-# LANGUAGE DuplicateRecordFields, Safe #-}

module SDL2.Data where


import Data.Word (Word8, Word16, Word32)

import Foreign

data SDLEvent = SDLEvent{
    eType :: Word32
} deriving (Show)

data SDLKeyboardEvent = SDL_KeyboardEvent{
    eventType :: Word32,
    timeStamp :: Word32,
    windowID :: Word32,
    state :: Word8,
    repeat :: Word8,
    keySys :: SDLKeySys
}

data SDLKeySys = SDLKeySys{
    scancode :: Word,
    keycode :: Word,
    mod :: Word16,
    unused :: Word32
}

instance Storable SDLEvent where
    sizeOf _    = 8
    alignment _ = 4
    peek p = do
        v <- peekByteOff p 0
        return (SDLEvent v)
    poke p (SDLEvent v) = pokeByteOff p 0 v
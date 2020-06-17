{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


import Foreign.Ptr (nullPtr)

import Graphics.Utilities

import Graphics.Vulkan.Instance


initialize :: IO ()
initialize = do
    let api = makeAPI 1 2 141
    app <- createVkApplicationInfo nullPtr "Nightmare" 0 "Nightmare" 0 api
    return ()

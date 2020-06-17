{-# LANGUAGE Safe #-}

module Graphics.Vulkan where


initialize :: IO ()
initialize = do
    let api = makeAPI 1 2 141
    return ()

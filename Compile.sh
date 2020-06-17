warnings="-Wall -Wcompat -Wname-shadowing"
flags="-XSafe -O2 -dynamic -threaded"
rts="-rtsopts"
sdl2="-lSDL2"
vulkan="-lvulkan -lVkLayer_core_validation"
ghc Main.hs $warnings $flags $vulkan $rts -outputdir ./Build/Release

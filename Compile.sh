warnings="-Wall -Wcompat -Wname-shadowing"
flags="-XSafe -O2 -dynamic -threaded"
rts="-rtsopts"
sdl2="-lSDL2"
vulkan="-lvulkan -lVkLayer_khronos_validation"

glslangValidator -V Shaders/Simple.comp -o Shaders/Simple.spv
ghc Main.hs $warnings $flags $sdl2 $vulkan $rts -outputdir ./Build/Release

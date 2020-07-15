warnings="-Wall -Wcompat -Wname-shadowing"
flags="-XSafe -O2 -dynamic -threaded"
rts="-rtsopts"
sdl2="-lSDL2"
vulkan="-lvulkan -lVkLayer_khronos_validation"
spirvFlags="-V120 --spirv-val"

glslangValidator $spirvFlags Shaders/Simple.vert -o Shaders/Simple.v.spv
glslangValidator $spirvFlags Shaders/Simple.frag -o Shaders/Simple.f.spv
glslangValidator $spirvFlags Shaders/Simple.comp -o Shaders/Simple.c.spv
ghc Main.hs $warnings $flags $sdl2 $vulkan $rts -outputdir ./Build/Release

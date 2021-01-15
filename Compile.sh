warnings="-Wall -Wcompat -Wname-shadowing"
flags="-XSafe -O2 -dynamic -threaded"
rts="-rtsopts"
sdl2="-lSDL2"
vulkan="-lvulkan -lVkLayer_khronos_validation"
spirvFlags="--target-env vulkan1.2 --target-env spirv1.5 --spirv-val"

glslangValidator $spirvFlags Shaders/Violet/Rasterizer/Simple.vert -o Shaders/Violet/Rasterizer/Simple.v.spv
glslangValidator $spirvFlags Shaders/Violet/Rasterizer/Simple.frag -o Shaders/Violet/Rasterizer/Simple.f.spv
glslangValidator $spirvFlags Shaders/Violet/Compute/Tracer.comp -o Shaders/Violet/Compute/Tracer.c.spv
ghc Main.hs $warnings $flags $sdl2 $vulkan $rts -outputdir ./Build/Release

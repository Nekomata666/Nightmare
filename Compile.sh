warnings="-Wall -Wcompat -Wname-shadowing"
flags="-XSafe -O2 -dynamic -threaded"
rts="-rtsopts"
sdl2="-lSDL2"
vulkan="-lvulkan -lVkLayer_khronos_validation"
spirvFlags="--target-env vulkan1.2 --target-env spirv1.5 --spirv-val"
violet="Shaders/Violet/"

# glslangValidator $spirvFlags ${violet}Rasterizer/Simple.vert -o ${violet}Rasterizer/Simple.v.spv
# glslangValidator $spirvFlags ${violet}Rasterizer/Simple.frag -o ${violet}Rasterizer/Simple.f.spv
glslangValidator $spirvFlags ${violet}Compute/Tracer.comp -o ${violet}Compute/Tracer.c.spv

ghc Main.hs $warnings $flags $sdl2 $vulkan $rts -outputdir ./Build/Release

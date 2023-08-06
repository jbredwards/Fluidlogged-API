package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import atomicstryker.dynamiclights.client.DynamicLights;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.*;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.Fluid;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static net.minecraft.util.EnumFacing.*;

/**
 * corrects a lot of FluidState related interactions
 * @author jbred
 *
 */
public final class PluginWorld implements IASMPlugin
{
    //special case local var shift for galaxy space
    boolean isGalaxySpace = false;

    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //setBlockState
        if(checkMethod(method, obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z")) {
            isGalaxySpace = !method.localVariables.isEmpty() && method.localVariables.get(0).index == 5;
            return 1;
        }

        //setBlockToAir
        else if(checkMethod(method, obfuscated ? "func_175698_g" : "setBlockToAir", null))
            return 2;

        //destroyBlock
        else if(checkMethod(method, obfuscated ? "func_175655_b" : "destroyBlock", null))
            return 2;

        //neighborChanged
        else if(checkMethod(method, obfuscated ? "func_190524_a" : "neighborChanged", null))
            return 3;

        //handleMaterialAcceleration
        else if(checkMethod(method, obfuscated ? "func_72918_a" : "handleMaterialAcceleration", null)) {
            return 4;
        }

        //isMaterialInBB
        else if(checkMethod(method, obfuscated ? "func_72875_a" : "isMaterialInBB", null)) {
            return 5;
        }

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(checkMethod(method, obfuscated ? "func_72953_d" : "containsAnyLiquid", null)
        || checkMethod(method, obfuscated ? "func_147470_e" : "isFlammableWithin", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null))
            return 6;

        //isFlammableWithin, fix bug with lava level
        else if(method.name.equals(obfuscated ? "func_147470_e" : "isFlammableWithin")) return 7;

        //fix neighbor brightness related bugs
        else if(checkMethod(method, obfuscated ? "func_175721_c" : "getLight", "(Lnet/minecraft/util/math/BlockPos;Z)I")
        || method.name.equals(obfuscated ? "func_175705_a" : "getLightFromNeighborsFor"))
            return 8;

        else if(method.name.equals(obfuscated ? "func_180500_c" : "checkLightFor")) return 9;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //setBlockState
        if(index == 1) {
            /*
             * setBlockState: (changes are around line 409)
             * Old code:
             * IBlockState oldState = getBlockState(pos);
             *
             * New code:
             * //optimize by calling from already cached chunk value, cause why not
             * IBlockState oldState = chunk.getBlockState(pos);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                if(obfuscated) ((MethodInsnNode)insn).name = "func_177435_g";
                ((MethodInsnNode)insn).owner = "net/minecraft/world/chunk/Chunk";
                ((VarInsnNode)getPrevious(insn, 2)).var = isGalaxySpace ? 5 : 4;
            }
            /*
             * setBlockState: (changes are around lines 410 & 411)
             * Old code:
             * int oldLight = oldState.getLightValue(this, pos);
             * int oldOpacity = oldState.getLightOpacity(this, pos);
             *
             * New code:
             * //cache FluidState light levels
             * int oldLight = Hooks.getLightValue(oldState, this, pos, chunk);
             * int oldOpacity = Hooks.getLightOpacity(oldState, this, pos, chunk);
             */
            else if(checkMethod(insn, "getLightValue") || checkMethod(insn, "getLightOpacity")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, isGalaxySpace ? 5 : 4));
                instructions.insertBefore(insn, genMethodNode(((MethodInsnNode)insn).name, "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)I"));
                instructions.remove(insn);
            }
            /*
             * setBlockState: (changes are around line 413)
             * Old code:
             * IBlockState iblockstate = chunk.setBlockState(pos, newState);
             *
             * New code:
             * //remove FluidState here if the new state can't be fluidlogged, or move fluid block here to FluidState
             * IBlockState iblockstate = chunk.setBlockState(pos, newState);
             * Hooks.handleOldFluidState(this, pos, chunk, oldState, newState, iblockstate, flags);
             */
            else if(checkMethod(insn, obfuscated ? "func_177436_a" : "setBlockState")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, isGalaxySpace ? 5 : 4));
                list.add(new VarInsnNode(ALOAD, isGalaxySpace ? 7 : 6));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, isGalaxySpace ? 10 : 9));
                list.add(new VarInsnNode(ILOAD, 3));
                list.add(genMethodNode("handleOldFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;I)V"));
                instructions.insert(insn.getNext(), list);
            }

            //end method transform
            else return checkMethod(insn, "markAndNotifyBlock");
        }
        /*
         * setBlockToAir & destroyBlock: (changes are around lines 468 & 492):
         * Old code:
         * return this.setBlockState(pos, Blocks.AIR.getDefaultState(), 3);
         *
         * New code:
         * //replace block here with FluidState here instead of air
         * return this.setBlockState(pos, FluidState.get(this, pos).getState(), 3);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidState", "get", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getState", "()Lnet/minecraft/block/state/IBlockState;", false));
            instructions.insert(insn, list);
            removeFrom(instructions, insn, -1);
            return true;
        }
        //neighborChanged
        else if(index == 3) {
            /*
             * neighborChanged: (changes are around line 634)
             * Old code:
             * IBlockState iblockstate = this.getBlockState(pos);
             *
             * New code:
             * //save chunk for later use
             * Chunk chunk = this.getChunk(pos);
             * IBlockState iblockstate = chunk.getBlockState(pos);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175726_f" : "getChunk", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false));
                list.add(new VarInsnNode(ASTORE, 15));
                list.add(new VarInsnNode(ALOAD, 15));
                instructions.insertBefore(getPrevious(insn, 2), list);
                //change getBlockState method
                instructions.remove(getPrevious(insn, 2));
                if(obfuscated) ((MethodInsnNode)insn).name = "func_177435_g";
                ((MethodInsnNode)insn).owner = "net/minecraft/world/chunk/Chunk";
            }
            /*
             * neighborChanged: (changes are around line 638)
             * Old code:
             * iblockstate.neighborChanged(this, pos, blockIn, fromPos);
             *
             * New code:
             * //update FluidStates
             * iblockstate.neighborChanged(this, pos, blockIn, fromPos);
             * Hooks.fluidNeighborChanged(this, pos, blockIn, fromPos, chunk);
             */
            else if(checkMethod(insn, obfuscated ? "func_189546_a" : "neighborChanged", null)) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, 3));
                list.add(new VarInsnNode(ALOAD, 15));
                list.add(genMethodNode("fluidNeighborChanged", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/chunk/Chunk;)V"));

                instructions.insert(insn, list);
                return true;
            }
        }
        /*
         * handleMaterialAcceleration: (changes are around line 2455):
         * Old code:
         * blockpos$pooledmutableblockpos.release();
         *
         * New code:
         * //account for FluidStates
         * blockpos$pooledmutableblockpos.release();
         * Pair flags = Hooks.handleFluidAcceleration(world, materialIn, entityIn, vec3d, flag, j2, k2, l2, i3, j3, k3);
         * flag = ((Boolean)flags.getKey()).booleanValue();
         * vec3d = (Vec3d)flags.getValue();
         */
        else if(index == 4 && checkMethod(insn, obfuscated ? "func_185344_t" : "release", "()V")) {
            final InsnList list = new InsnList();
            //handleFluidAcceleration
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            //vec3d
            list.add(new VarInsnNode(ALOAD, 11));
            //flag
            list.add(new VarInsnNode(ILOAD, 10));
            //aabb positions
            list.add(new VarInsnNode(ILOAD, 4));
            list.add(new VarInsnNode(ILOAD, 5));
            list.add(new VarInsnNode(ILOAD, 6));
            list.add(new VarInsnNode(ILOAD, 7));
            list.add(new VarInsnNode(ILOAD, 8));
            list.add(new VarInsnNode(ILOAD, 9));
            //adds new code
            list.add(genMethodNode("handleFluidAcceleration", "(Lnet/minecraft/world/World;Lnet/minecraft/block/material/Material;Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/Vec3d;ZIIIIII)Lorg/apache/commons/lang3/tuple/Pair;"));
            list.add(new VarInsnNode(ASTORE, 22));
            //flags.getKey()
            list.add(new VarInsnNode(ALOAD, 22));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "org/apache/commons/lang3/tuple/Pair", "getKey", "()Ljava/lang/Object;", false));
            list.add(new TypeInsnNode(CHECKCAST, "java/lang/Boolean"));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false));
            list.add(new VarInsnNode(ISTORE, 10));
            //flags.getValue()
            list.add(new VarInsnNode(ALOAD, 22));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "org/apache/commons/lang3/tuple/Pair", "getValue", "()Ljava/lang/Object;", false));
            list.add(new TypeInsnNode(CHECKCAST, "net/minecraft/util/math/Vec3d"));
            list.add(new VarInsnNode(ASTORE, 11));

            instructions.insert(insn, list);
            return true;
        }
        /*
         * isMaterialInBB: (changes are around line 2506)
         * Old code:
         * return false;
         *
         * New code:
         * //check FluidStates
         * return Hooks.isMaterialInFluidBB(this, bb, materialIn, j2, k2, l2, i3, j3, k3);
         */
        else if(index == 5 && insn.getOpcode() == ICONST_0) {
            final InsnList list = new InsnList();
            //params
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            //aabb positions
            list.add(new VarInsnNode(ILOAD, 3));
            list.add(new VarInsnNode(ILOAD, 4));
            list.add(new VarInsnNode(ILOAD, 5));
            list.add(new VarInsnNode(ILOAD, 6));
            list.add(new VarInsnNode(ILOAD, 7));
            list.add(new VarInsnNode(ILOAD, 8));
            //adds new code
            list.add(genMethodNode("isMaterialInFluidBB", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;IIIIII)Z"));

            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(index == 6 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * isFlammableWithin: (changes are around line 2379)
         * Old code:
         * if (block == Blocks.FIRE || block == Blocks.FLOWING_LAVA || block == Blocks.LAVA)
         * {
         *     ...
         * }
         *
         * New code:
         * //account for FluidStates
         * if (block == Blocks.FIRE || Hooks.isFlammableFluidWithin(block, this, blockpos$pooledmutableblockpos, bb))
         * {
         *     ...
         * }
         */
        else if(index == 7 && checkField(insn, obfuscated ? "field_150353_l" : "LAVA")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 8));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("isFlammableFluidWithin", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Z"));
            instructions.insert(insn, list);
            removeFrom(instructions, insn, -3);
            return true;
        }
        /*
         * getLight & getLightFromNeighborsFor: (changes around lines 768 & 899):
         * Old code:
         * if (checkNeighbors && this.getBlockState(pos).useNeighborBrightness())
         * {
         *     ...
         * }
         *
         * New code:
         * //fix neighbor brightness related bugs
         * if (checkNeighbors && Hooks.useNeighborBrightness(this, pos))
         * {
         *     ...
         * }
         */
        else if(index == 8 && checkMethod(insn, obfuscated ? "func_185916_f" : "useNeighborBrightness")) {
            instructions.insert(insn, genMethodNode("useNeighborBrightness", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            removeFrom(instructions, insn, -1);
            return true;
        }
        /*
         * checkLightFor: (changes are around line 3165)
         * Old code:
         * IBlockState bs = this.getBlockState(blockpos$pooledmutableblockpos);
         * int i7 = Math.max(1, bs.getBlock().getLightOpacity(bs, this, blockpos$pooledmutableblockpos));
         *
         * New code:
         * //use forge-added opacity getter
         * IBlockState bs = null;
         * int i7 = Math.max(1, this.getBlockLightOpacity(blockpos$pooledmutableblockpos));
         */
        else if(index == 9) {
            //don't collect block state here, it's unused
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, new InsnNode(ACONST_NULL));
                removeFrom(instructions, insn, -2);
            }
            else if(checkMethod(insn, "getLightOpacity")) {
                removeFrom(instructions, getPrevious(insn, 3), -2);
                instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", "getBlockLightOpacity", "(Lnet/minecraft/util/math/BlockPos;)I", false));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/asm/impl/IChunkProvider");
        addMethod(classNode, "getChunkFromBlockCoords", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175726_f" : "getChunk", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false);
        });
        //rayTraceBlocks, ray traces now include fluidlogged fluid blocks
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_147447_a" : "rayTraceBlocks", "(Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;"),
            "rayTraceBlocks", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ILOAD, 5);
            }
        );
        /*
         * getRawLight:
         * New code:
         * //account for FluidStates when calculating raw light & opacity values
         * private int getRawLight(BlockPos pos, EnumSkyBlock lightType)
         * {
         *     return Hooks.getRawLight(this, pos, lightType);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175638_a" : "getRawLight"),
            "getRawLight", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/EnumSkyBlock;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * isAirBlock:
         * New code:
         * //increase performance
         * public boolean isAirBlock(BlockPos pos)
         * {
         *     return Hooks.isAirBlock(this, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_175623_d" : "isAirBlock"),
            "isAirBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            }
        );

        return true;
    }

    @Override
    public boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) {
        if(index == 3) {
            method.localVariables.add(new LocalVariableNode("chunk", "Lnet/minecraft/world/chunk/Chunk;", null, start, end, 15));
            return true;
        }

        else if(index == 4) {
            method.localVariables.add(new LocalVariableNode("flags", "Lorg/apache/commons/lang3/tuple/Pair;", null, start, end, 22));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void fluidNeighborChanged(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Block blockIn, @Nonnull BlockPos fromPos, @Nonnull Chunk chunk) {
            final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            if(!fluidState.isEmpty()) fluidState.getState().neighborChanged(world, pos, blockIn, fromPos);
        }

        public static int getLightOpacity(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return PluginChunk.Hooks.getFluidLightOpacity(state, world, pos, chunk);
        }

        public static int getLightValue(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk) {
            return PluginChunk.Hooks.getFluidLightValue(state, world, pos, chunk);
        }

        public static int getRawLight(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumSkyBlock lightType) {
            if(lightType == EnumSkyBlock.SKY && world.canSeeSky(pos)) return 15;
            final Chunk chunk = world.getChunk(pos);
            final IBlockState state = chunk.getBlockState(pos);
            final IBlockState fluidState = FluidState.getFromProvider(chunk, pos).getState();
            int light = lightType == EnumSkyBlock.SKY ? 0 : FluidloggedAPI.isDynamicLights
                    ? DLHooks.getLightValue(state, world, pos, fluidState)
                    : Math.max(state.getLightValue(world, pos), fluidState.getLightValue(world, pos));
            int opacity = Math.max(Math.max(state.getLightOpacity(world, pos), fluidState.getLightOpacity(world, pos)), 1);
            if(opacity >= 15) return light; // Forge: fix MC-119932
            else if(light >= 14) return light;

            for(EnumFacing facing : values()) {
                final BlockPos offset = pos.offset(facing);
                final int neighborLight = world.getLightFor(lightType, offset) - opacity;

                if(neighborLight > light) light = neighborLight;
                if(light >= 14) return light;
            }

            return light;
        }

        @Nonnull
        public static Pair<Boolean, Vec3d> handleFluidAcceleration(@Nonnull World world, @Nonnull Material material, @Nonnull Entity entity, @Nonnull Vec3d vec3dIn, boolean flagIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
            for(int x = minX; x < maxX; x++) {
                for(int y = minY; y < maxY; y++) {
                    for(int z = minZ; z < maxZ; z++) {
                        final BlockPos pos = new BlockPos(x, y, z);
                        final FluidState fluidState = FluidState.get(world, pos);
                        if(!fluidState.isEmpty()) {
                            final Block block = fluidState.getBlock();
                            final @Nullable Boolean result = block.isEntityInsideMaterial(world, pos, fluidState.getState(), entity, maxY, material, false);
                            if(Boolean.TRUE.equals(result)) {
                                vec3dIn = block.modifyAcceleration(world, pos, entity, vec3dIn);
                                flagIn = true;
                            }

                            else if(!Boolean.FALSE.equals(result) && fluidState.getMaterial() == material) {
                                //check for fluid height
                                if(maxY >= y + 1 - (1 / 9.0f)) {
                                    vec3dIn = block.modifyAcceleration(world, pos, entity, vec3dIn);
                                    flagIn = true;
                                }
                            }
                        }
                    }
                }
            }

            return Pair.of(flagIn, vec3dIn);
        }

        public static void handleOldFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Chunk chunk, @Nonnull IBlockState oldState, @Nonnull IBlockState newState, @Nullable IBlockState iblockstate, int blockFlags) {
            if(iblockstate != null) {
                final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
                //this mod adds a special flag (x | 32, example: Constants.BlockFlags.DEFAULT | 32) that removes any FluidState here
                if((blockFlags & 32) != 0) {
                    if(!fluidState.isEmpty()) FluidloggedUtils.setFluidState(world, pos, newState, FluidState.EMPTY, false, false, blockFlags);
                    return;
                }

                //if the new state isn't fluidloggable, remove the FluidState here
                if(!fluidState.isEmpty()) {
                    if(!FluidloggedUtils.isStateFluidloggable(newState, world, pos, fluidState.getFluid())) {
                        FluidloggedUtils.setFluidState(world, pos, newState, FluidState.EMPTY, false, false, blockFlags);
                        if(world.isAreaLoaded(pos, 1)) FluidloggedUtils.notifyFluids(world, pos, fluidState, false);
                    }

                    //ensure fluids are updated when the block here changes
                    else if(world.isAreaLoaded(pos, 1)) FluidloggedUtils.notifyFluids(world, pos, fluidState, true);
                    return;
                }

                //save oldState as FluidState if possible
                final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromState(oldState);
                if(fluid != null && FluidloggedUtils.isFluidloggableFluid(oldState, world, pos) && FluidloggedUtils.isStateFluidloggable(newState, world, pos, fluid))
                    FluidloggedUtils.setFluidState(world, pos, newState, FluidState.of(fluid), false, false, blockFlags);
            }
        }

        public static boolean isAirBlock(@Nonnull World world, @Nonnull BlockPos pos) {
            final IBlockState state = world.getBlockState(pos);
            return state.getBlock().isAir(state, world, pos);
        }

        public static boolean isFlammableFluidWithin(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
            if(block.getDefaultState().getMaterial() == Material.LAVA) return Boolean.TRUE.equals(block.isAABBInsideLiquid(world, pos, bb));
            final FluidState fluidState = FluidState.get(world, pos); //handle possible lava FluidState
            return fluidState.getMaterial() == Material.LAVA && Boolean.TRUE.equals(fluidState.getBlock().isAABBInsideLiquid(world, pos, bb));
        }

        public static boolean isMaterialInFluidBB(@Nonnull World world, @Nonnull AxisAlignedBB bb, @Nonnull Material materialIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
            for(int x = minX; x < maxX; ++x) {
                for(int y = minY; y < maxY; ++y) {
                    for(int z = minZ; z < maxZ; ++z) {
                        final BlockPos pos = new BlockPos(x, y, z);
                        final FluidState fluidState = FluidState.get(world, pos);

                        if(!fluidState.isEmpty()) {
                            @Nullable Boolean result = fluidState.getBlock().isAABBInsideMaterial(world, pos, bb, materialIn);
                            if(result != null) {
                                if(!result) continue;
                                return true;
                            }
                            else if(fluidState.getMaterial() == materialIn)
                                return true;
                        }
                    }
                }
            }

            return false;
        }

        @SuppressWarnings("ConstantConditions")
        @Nullable
        public static RayTraceResult rayTraceBlocks(@Nonnull World world, @Nonnull Vec3d vec, @Nonnull Vec3d end, boolean stopOnLiquid, boolean ignoreBlockWithoutBoundingBox, boolean returnLastUncollidableBlock) {
            if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z) || Double.isNaN(end.x) || Double.isNaN(end.y) || Double.isNaN(end.z))
                return null;

            final int endX = MathHelper.floor(end.x);
            final int endY = MathHelper.floor(end.y);
            final int endZ = MathHelper.floor(end.z);
            int prevX = MathHelper.floor(vec.x);
            int prevY = MathHelper.floor(vec.y);
            int prevZ = MathHelper.floor(vec.z);

            BlockPos pos = new BlockPos(prevX, prevY, prevZ);
            RayTraceResult result;

            //check FluidState
            if(stopOnLiquid) {
                final FluidState fluidState = FluidState.get(world, pos);
                if(!fluidState.isEmpty() && fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                    result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                    if(result != null) { return result; }
                }
            }

            IBlockState state = world.getBlockState(pos);
            if(state.getBlock().canCollideCheck(state, stopOnLiquid) && (stopOnLiquid && FluidloggedUtils.isFluid(state) || !ignoreBlockWithoutBoundingBox || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB)) {
                result = state.collisionRayTrace(world, pos, vec, end);
                if(result != null) { return result; }
            }

            RayTraceResult lastResult = FluidloggedAPI.isChiseledMe ? new RayTraceResult(RayTraceResult.Type.MISS, end, EnumFacing.DOWN, pos) : null;
            for(int i = 200; i-- >= 0;) {
                if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z))
                    return null;

                if(prevX == endX && prevY == endY && prevZ == endZ)
                    return returnLastUncollidableBlock ? lastResult : null;

                boolean flagX = true, flagY = true, flagZ = true;
                double x = 999, y = 999, z = 999;

                if(endX > prevX) x = prevX + 1;
                else if(endX < prevX) x = prevX;
                else flagX = false;

                if(endY > prevY) y = prevY + 1;
                else if(endY < prevY) y = prevY;
                else flagY = false;

                if(endZ > prevZ) z = prevZ + 1;
                else if(endZ < prevZ) z = prevZ;
                else flagZ = false;

                double coveredX = 999, coveredY = 999, coveredZ = 999;
                double distX = end.x - vec.x;
                double distY = end.y - vec.y;
                double distZ = end.z - vec.z;

                if(flagX) coveredX = (x - vec.x) / distX;
                if(flagY) coveredY = (y - vec.y) / distY;
                if(flagZ) coveredZ = (z - vec.z) / distZ;

                if(coveredX == -0) coveredX = -1.0E-4;
                if(coveredY == -0) coveredY = -1.0E-4;
                if(coveredZ == -0) coveredZ = -1.0E-4;

                //the general direction of the trace
                EnumFacing facing;

                if(coveredX < coveredY && coveredX < coveredZ) {
                    facing = endX > prevX ? WEST : EAST;
                    vec = new Vec3d(x, vec.y + distY * coveredX, vec.z + distZ * coveredX);
                }

                else if(coveredY < coveredZ) {
                    facing = endY > prevY ? DOWN : UP;
                    vec = new Vec3d(vec.x + distX * coveredY, y, vec.z + distZ * coveredY);
                }

                else {
                    facing = endZ > prevZ ? NORTH : SOUTH;
                    vec = new Vec3d(vec.x + distX * coveredZ, vec.y + distY * coveredZ, z);
                }

                prevX = MathHelper.floor(vec.x) - (facing == EAST  ? 1 : 0);
                prevY = MathHelper.floor(vec.y) - (facing == UP    ? 1 : 0);
                prevZ = MathHelper.floor(vec.z) - (facing == SOUTH ? 1 : 0);

                pos = new BlockPos(prevX, prevY, prevZ);

                //check FluidState
                if(stopOnLiquid) {
                    FluidState fluidState = FluidState.get(world, pos);
                    if(!fluidState.isEmpty()) {
                        if(fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                            result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                            if(result != null) return result;
                        }

                        else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                    }
                }

                state = world.getBlockState(pos);
                if(!ignoreBlockWithoutBoundingBox || state.getMaterial() == Material.PORTAL || stopOnLiquid && FluidloggedUtils.isFluid(state) || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB) {
                    if(state.getBlock().canCollideCheck(state, stopOnLiquid)) {
                        result = state.collisionRayTrace(world, pos, vec, end);
                        if(result != null) return result;
                    }

                    else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                }
            }

            return returnLastUncollidableBlock ? lastResult : null;
        }

        public static boolean useNeighborBrightness(@Nonnull World world, @Nonnull BlockPos pos) {
            return PluginChunkCache.Hooks.useNeighborBrightness(world, pos);
        }
    }

    //hold Dynamic Lights methods in separate class to avoid crash
    public static final class DLHooks
    {
        public static int getLightValue(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState fluidState) {
            return Math.max(DynamicLights.getLightValue(state.getBlock(), state, world, pos), fluidState.getLightValue(world, pos));
        }
    }
}

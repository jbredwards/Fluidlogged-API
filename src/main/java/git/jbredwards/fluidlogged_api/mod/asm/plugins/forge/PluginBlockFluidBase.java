/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IChunkProvider;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidExtendedStateHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl.SpecializedFluidNeighborInfo;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * modded fluids work properly with the mod & prevent startup crash
 * @author jbred
 *
 */
public final class PluginBlockFluidBase implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "<init>", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V")) return 1;
        else if(checkMethod(method, "<clinit>", "()V")) return 2;
        else if(method.name.equals("getFlowDirection") || method.name.equals("getDensity") || method.name.equals("getTemperature")) return 4;
        else if(method.name.equals("getFluid")) return 5;
        else return method.name.equals("getFogColor") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * A bit of context, this mod makes BlockLiquid implement several IUnlistedProperties, and since
         * those properties are initialized in forge's BlockFluidBase class, forge's class gets loaded early.
         * This means that the blocks defined in BlockFluidBase#defaultDisplacements aren't initialized yet,
         * which causes the game to crash. This transformation moves the defaultDisplacements initialization
         * from a static initializer to one in the class's constructor. This transformer insures the game
         * doesn't crash on startup.
         */
        if(index == 1 && checkField(insn, "defaultDisplacements", "Ljava/util/Map;")) {
            instructions.insert(insn, genMethodNode("defaultDisplacements", "(Ljava/util/Map;)Ljava/util/Map;"));
            return true;
        }
        /*
         * Removes the static initializer for BlockFluidBase#defaultPlacements, reason mentioned above.
         */
        else if(index == 2 && insn.getNext().getOpcode() == LDC) {
            //removes all default entries, as the class is now loaded before they're registered
            while(insn.getPrevious().getOpcode() != PUTSTATIC) instructions.remove(insn.getPrevious());
            return true;
        }
        /*
         * getFogColor: (changes are around line 840)
         * Old code:
         * if (!isWithinFluid(world, pos, ActiveRenderInfo.projectViewFromEntity(entity, partialTicks)))
         * {
         *     ...
         * }
         *
         * New code:
         * //don't use this code
         * if (!true)
         * {
         *     ...
         * }
         */
        else if(index == 3 && checkMethod(insn, "isWithinFluid")) {
            instructions.insert(insn, new InsnNode(ICONST_1));
            removeFrom(instructions, insn, -6);
            return true;
        }
        /*
         * getFlowDirection, getDensity, and getTemperature: (changes are around lines 653, 594, and 612)
         * Old code:
         * IBlockState state = world.getBlockState(pos);
         *
         * New code:
         * //ensure that FluidStates render their flow vectors
         * IBlockState state = FluidloggedUtils.getFluidOrReal(world, pos);
         */
        else if(index == 4 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * getFluid: (changes are around line 810)
         * Old code:
         * return FluidRegistry.getFluid(fluidName);
         *
         * New code:
         * //exposed to significantly boost performance, sorry forge devs but why'd you not want this exposed again?
         * return this.definedFluid;
         */
        else if(index == 5 && checkMethod(insn, "getFluid", "(Ljava/lang/String;)Lnet/minecraftforge/fluids/Fluid;")) {
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * shouldSideBeRendered:
         * New code:
         * //this does a few things differently:
         * //1. check for neighboring FluidStates rather than neighboring IBlockStates
         * //2. check for compatible fluids rather than just ones with an identical Material
         * //3. check for neighboring fluids this is connected to
         * @Override
         * public boolean shouldSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side)
         * {
         *     return Hooks.shouldFluidSideBeRendered(state, world, pos, this.densityDir);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"),
            "shouldFluidSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
            }
        );
        /*
         * getExtendedState:
         * New code:
         * //fix corner heights and some vanilla 1.13+ inconsistencies
         * @Override
         * @Nonnull
         * public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos)
         * {
         *     return Hooks.getFluidExtendedState(world, pos, oldState);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getExtendedState"),
            "getFluidExtendedState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 1);
            }
        );
        /*
         * getFlowVector:
         * New code:
         * //don't flow into/from invalid sides
         * public Vec3d getFlowVector(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.getFluidFlowVector(world, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getFlowVector"),
            "getFluidFlowVector", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * hasVerticalFlow:
         * New code:
         * //don't flow into/from invalid sides
         * final boolean hasVerticalFlow(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.hasVerticalFlow(world, pos, this.getFluid(), this.densityDir);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("hasVerticalFlow"),
            "hasVerticalFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
            }
        );
        /*
         * getStateAtViewpoint:
         * New code:
         * //use improved fluid collision
         * @Override
         * public IBlockState getStateAtViewpoint(IBlockState state, IBlockAccess world, BlockPos pos, Vec3d viewpoint)
         * {
         *     return FluidCollisionHandler.getStateAtViewpoint(state, world, pos, viewpoint);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getStateAtViewpoint"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidCollisionHandler", "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;", false);
        });
        /*
         * isEntityInsideMaterial:
         * New code:
         * //better entity fluid collision
         * @ASMGenerated
         * public Boolean isEntityInsideMaterial(IBlockAccess world, BlockPos blockpos, IBlockState iblockstate, Entity entity, double yToTest, Material materialIn, boolean testingHead)
         * {
         *     return FluidCollisionHandler.isEntityInsideFluid(world, blockpos, iblockstate, entity, yToTest, materialIn, testingHead);
         * }
         */
        addMethod(classNode, "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitVarInsn(DLOAD, 5);
            generator.visitVarInsn(ALOAD, 7);
            generator.visitVarInsn(ILOAD, 8);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidCollisionHandler", "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", false);
        });
        /*
         * isAABBInsideMaterial:
         * New code:
         * //better entity fluid collision
         * @ASMGenerated
         * public Boolean isAABBInsideMaterial(World world, BlockPos pos, AxisAlignedBB boundingBox, Material materialIn)
         * {
         *     return FluidCollisionHandler.isAABBInsideMaterial(this.material, world, pos, boundingBox, materialIn);
         * }
         */
        addMethod(classNode, "isAABBInsideMaterial", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidCollisionHandler", "isAABBInsideMaterial", "(Lnet/minecraft/block/material/Material;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;", false);
        });
        /*
         * isAABBInsideLiquid:
         * New code:
         * //better entity fluid collision
         * @ASMGenerated
         * public Boolean isAABBInsideLiquid(World world, BlockPos pos, AxisAlignedBB boundingBox)
         * {
         *     return FluidCollisionHandler.isAABBWithinLiquid(world, pos, boundingBox);
         * }
         */
        addMethod(classNode, "isAABBInsideLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidCollisionHandler", "isAABBInsideLiquid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", false);
        });
        /*
         * Accessor:
         * New code:
         * //add public accessor for private field
         * @ASMGenerated
         * public float getDensityDir_Public()
         * {
         *     return this.densityDir;
         * }
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidBase$Accessor");
        addMethod(classNode, "getDensityDir_Public", "()I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
        });
        /*
         * Accessor:
         * New code:
         * //add public accessor for private field
         * @ASMGenerated
         * public float getQuantaPerBlockFloat_Public()
         * {
         *     return this.quantaPerBlockFloat;
         * }
         */
        addMethod(classNode, "getQuantaPerBlockFloat_Public", "()F", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlockFloat", "F");
        });
        /*
         * Accessor:
         * New code:
         * //add public accessor for private field
         * @ASMGenerated
         * public int getQuantaPerBlock_Public()
         * {
         *     return this.quantaPerBlock;
         * }
         */
        addMethod(classNode, "getQuantaPerBlock_Public", "()I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
        });
        /*
         * Accessor:
         * New code:
         * //add public accessor for private field
         * @ASMGenerated
         * public float getQuantaFraction_Public()
         * {
         *     return this.quantaFraction;
         * }
         */
        addMethod(classNode, "getQuantaFraction_Public", "()F", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F");
        });
        /*
         * Accessor:
         * New code:
         * //add public accessor for private field
         * @ASMGenerated
         * public Map<Block, Boolean> getDisplacements_Public()
         * {
         *     return this.displacements;
         * }
         */
        addMethod(classNode, "getDisplacements_Public", "()Ljava/util/Map;", "()Ljava/util/Map<Lnet/minecraft/block/Block;Ljava/lang/Boolean;>;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "displacements", "Ljava/util/Map;");
        });

        return true;
    }

    @Override
    public boolean recalcFrames(final boolean obfuscated) { return true; }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Map<Block, Boolean> defaultDisplacements(@Nonnull Map<Block, Boolean> map) {
            final Map<Block, Boolean> ret = new HashMap<>();
            //restore old entries
            ret.put(Blocks.OAK_DOOR,                       false);
            ret.put(Blocks.SPRUCE_DOOR,                    false);
            ret.put(Blocks.BIRCH_DOOR,                     false);
            ret.put(Blocks.JUNGLE_DOOR,                    false);
            ret.put(Blocks.ACACIA_DOOR,                    false);
            ret.put(Blocks.DARK_OAK_DOOR,                  false);
            ret.put(Blocks.TRAPDOOR,                       false);
            ret.put(Blocks.IRON_TRAPDOOR,                  false);
            ret.put(Blocks.OAK_FENCE,                      false);
            ret.put(Blocks.SPRUCE_FENCE,                   false);
            ret.put(Blocks.BIRCH_FENCE,                    false);
            ret.put(Blocks.JUNGLE_FENCE,                   false);
            ret.put(Blocks.DARK_OAK_FENCE,                 false);
            ret.put(Blocks.ACACIA_FENCE,                   false);
            ret.put(Blocks.NETHER_BRICK_FENCE,             false);
            ret.put(Blocks.OAK_FENCE_GATE,                 false);
            ret.put(Blocks.SPRUCE_FENCE_GATE,              false);
            ret.put(Blocks.BIRCH_FENCE_GATE,               false);
            ret.put(Blocks.JUNGLE_FENCE_GATE,              false);
            ret.put(Blocks.DARK_OAK_FENCE_GATE,            false);
            ret.put(Blocks.ACACIA_FENCE_GATE,              false);
            ret.put(Blocks.WOODEN_PRESSURE_PLATE,          false);
            ret.put(Blocks.STONE_PRESSURE_PLATE,           false);
            ret.put(Blocks.LIGHT_WEIGHTED_PRESSURE_PLATE,  false);
            ret.put(Blocks.HEAVY_WEIGHTED_PRESSURE_PLATE,  false);
            ret.put(Blocks.LADDER,                         false);
            ret.put(Blocks.IRON_BARS,                      false);
            ret.put(Blocks.GLASS_PANE,                     false);
            ret.put(Blocks.STAINED_GLASS_PANE,             false);
            ret.put(Blocks.PORTAL,                         false);
            ret.put(Blocks.END_PORTAL,                     false);
            ret.put(Blocks.COBBLESTONE_WALL,               false);
            ret.put(Blocks.BARRIER,                        false);
            ret.put(Blocks.STANDING_BANNER,                false);
            ret.put(Blocks.WALL_BANNER,                    false);
            ret.put(Blocks.CAKE,                           false);
            ret.put(Blocks.IRON_DOOR,                      false);
            ret.put(Blocks.STANDING_SIGN,                  false);
            ret.put(Blocks.WALL_SIGN,                      false);
            ret.put(Blocks.REEDS,                          false);
            //new entries added by other mods (never actually seen mods do this, but just in case)
            ret.putAll(map);

            return ret;
        }

        @Nonnull
        public static IBlockState getFluidExtendedState(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull final IBlockState state) {
            return FluidExtendedStateHandler.getExtendedState(state, new SpecializedFluidNeighborInfo.Forge(world, pos, FluidState.of(state), 1), info ->
                    FluidFlowHandler.getFlowAngle(((BlockFluidBase)info.getOrigin().getBlock()).getFlowVector(info.getCache(), pos)));
        }

        @Nonnull
        public static Vec3d getFluidFlowVector(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            return FluidFlowHandler.getFlowVec(new SpecializedFluidNeighborInfo.Forge(world, pos, FluidloggedUtils.getFluidState(world, pos), 1));
        }

        public static boolean hasVerticalFlow(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final Fluid fluid, final int densityDir) {
            @Nonnull final EnumFacing facing = (densityDir < 0) ? EnumFacing.UP : EnumFacing.DOWN;
            if(!FluidloggedUtils.canFluidFlow(world, pos, world.getBlockState(pos), facing)) return false;

            @Nonnull final BlockPos offset = pos.down(densityDir);
            if(world instanceof IChunkProvider) {
                @Nullable final Chunk chunk = ((IChunkProvider)world).getChunk(pos);
                if(chunk != null) {
                    @Nonnull final IBlockState state = chunk.getBlockState(offset);
                    return FluidloggedUtils.canFluidFlow(world, offset, state, facing.getOpposite())
                            && FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(chunk, offset, state).getFluid(), fluid);
                }
            }

            @Nonnull final IBlockState state = world.getBlockState(offset);
            return FluidloggedUtils.canFluidFlow(world, offset, state, facing.getOpposite())
                    && FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(world, offset, state).getFluid(), fluid);
        }

        public static boolean shouldFluidSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side, int densityDir) {
            if(!FluidloggedUtils.canFluidFlow(world, pos, world.getBlockState(pos), side)) return true;
            final Fluid fluid = FluidloggedUtils.getFluidFromState(state);

            final BlockPos offset = pos.offset(side);
            final IBlockState neighbor = world.getBlockState(offset);

            //this check exists for mods like coral reef that don't have proper block sides
            if(FluidloggedUtils.isCompatibleFluid(fluid, FluidloggedUtils.getFluidFromState(neighbor))) return false;
            else if(side != (densityDir > 0 ? EnumFacing.DOWN : EnumFacing.UP) && neighbor.doesSideBlockRendering(world, offset, side.getOpposite())) return false;
            return !FluidloggedUtils.canFluidFlow(world, offset, neighbor, side.getOpposite()) || !FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(world, offset, neighbor).getFluid(), fluid);
        }
    }

    public interface Accessor
    {
        int getDensityDir_Public();
        int getQuantaPerBlock_Public();
        float getQuantaFraction_Public();
        float getQuantaPerBlockFloat_Public();

        @Nonnull
        Map<Block, Boolean> getDisplacements_Public();
    }
}

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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidExtendedStateHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.ISpecializedFluidNeighborInfo;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl.SpecializedFluidNeighborInfo;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumBlockRenderType;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.EnumParticleTypes;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * makes liquids fluidloggable
 * @author jbred
 *
 */
public final class PluginBlockLiquid implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_149645_b" : "getRenderType")) return 1;
        else return method.name.equals(obfuscated ? "func_180655_c" : "randomDisplayTick") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getRenderType: (changes are around line 168)
         * Old code:
         * return EnumBlockRenderType.LIQUID;
         *
         * New code:
         * //change render type, so it can be handled by forge's fluid rendering system
         * return Hooks.getRenderType(EnumBlockRenderType.LIQUID);
         */
        if(index == 1 && insn.getOpcode() == ARETURN) {
            instructions.insertBefore(insn, genMethodNode("getRenderType", "(Lnet/minecraft/util/EnumBlockRenderType;)Lnet/minecraft/util/EnumBlockRenderType;"));
            return true;
        }
        /*
         * randomDisplayTick: (changes are around line 415)
         * Old code:
         * if (rand.nextInt(10) == 0 && worldIn.getBlockState(pos.down()).isTopSolid())
         * {
         *     ...
         * }
         *
         * New code:
         * //fix bugs with drip particles
         * if (rand.nextInt(10) == 0 && Hooks.spawnDripParticles(stateIn, worldIn, pos, rand))
         * {
         *     ...
         * }
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_185896_q" : "isTopSolid")) {
            instructions.insert(insn, genMethodNode("spawnDripParticles", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Ljava/util/Random;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 4));
            instructions.insert(insn, new VarInsnNode(ALOAD, 3));
            instructions.insert(insn, new VarInsnNode(ALOAD, 2));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            removeFrom(instructions, insn, -4);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/fluid/IFlowCostFluid");
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/fluid/IFluidloggableFluid");
        classNode.interfaces.add("net/minecraftforge/fluids/IFluidBlock");
        // bounds check is handled via getStateAtViewpoint, so BlockLiquid::getFogColor can be removed
        classNode.methods.removeIf(method -> method.name.equals("getFogColor"));
        /*
         * createBlockState:
         * New code:
         * //use forge unlisted fluid props
         * protected BlockStateContainer createBlockState()
         * {
         *     return Hooks.createLiquidBlockState(this);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180661_e" : "createBlockState"),
            "createLiquidBlockState", "(Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/BlockStateContainer;",
                generator -> generator.visitVarInsn(ALOAD, 0)
        );
        /*
         * shouldSideBeRendered:
         * New code:
         * //better side rendering
         * @SideOnly(Side.CLIENT)
         * public boolean shouldSideBeRendered(IBlockState blockState, IBlockAccess blockAccess, BlockPos pos, EnumFacing side)
         * {
         *     return PluginBlockFluidBase.Hooks.shouldFluidSideBeRendered(blockState, blockAccess, pos, side, -1);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitInsn(ICONST_M1);
            generator.visitMethodInsn(INVOKESTATIC, getFluidHookClass(), "shouldFluidSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;I)Z", false);
        });
        /*
         * getFlow:
         * New code:
         * //fix canFluidFlow-related vector bugs
         * public Vec3d getFlow(IBlockAccess worldIn, BlockPos pos, IBlockState state)
         * {
         *     return Hooks.getFlow(worldIn, pos, state);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_189543_a" : "getFlow"),
            "getFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        /*
         * getBlockLiquidHeight:
         * New code:
         * //use the correct block liquid height
         * public static float getBlockLiquidHeight(IBlockState state, IBlockAccess worldIn, BlockPos pos)
         * {
         *     return Hooks.getBlockLiquidHeight(state, worldIn, pos);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_190973_f" : "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F"),
            "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * checkForMixing:
         * New code:
         * //
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176365_e" : "checkForMixing"),
            "checkForMixing", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        /*
         * getExtendedState:
         * New code:
         * //fix corner heights
         * @ASMGenerated
         * public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos)
         * {
         *     return Hooks.getLiquidExtendedState(world, pos, oldState);
         * }
         */
        addMethod(classNode, "getExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;",
            "getLiquidExtendedState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 1);
            }
        );
        /*
         * getStateAtViewpoint:
         * New code:
         * //make this more accurate with fluid height
         * @ASMGenerated
         * public IBlockState getStateAtViewpoint(IBlockState state, IBlockAccess world, BlockPos pos, Vec3d viewpoint)
         * {
         *     return FluidCollisionHandler.getStateAtViewpoint(state, world, pos, viewpoint);
         * }
         */
        addMethod(classNode, "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidCollisionHandler", "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;", false);
        });
        /*
         * isEntityInsideMaterial:
         * New code:
         * //make this more accurate with fluid height
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
         * //make this more accurate with fluid height
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
         * //make this more accurate with fluid height
         * @ASMGenerated
         * public Boolean isAABBInsideLiquid(World world, BlockPos pos, AxisAlignedBB boundingBox)
         * {
         *     return FluidCollisionHandler.isAABBInsideLiquid(world, pos, boundingBox);
         * }
         */
        addMethod(classNode, "isAABBInsideLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/mod/common/fluid/handler/FluidCollisionHandler", "isAABBInsideLiquid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", false);
        });
        /*
         * getFluid:
         * New code:
         * //IFluidBlock implementation
         * @ASMGenerated
         * public Fluid getFluid()
         * {
         *     return Hooks.getLiquid(this.material);
         * }
         */
        addMethod(classNode, "getFluid", "()Lnet/minecraftforge/fluids/Fluid;",
            "getLiquid", "(Lnet/minecraft/block/material/Material;)Lnet/minecraftforge/fluids/Fluid;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
            }
        );
        /*
         * place:
         * New code:
         * //IFluidBlock implementation
         * @ASMGenerated
         * public int place(World world, BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace)
         * {
         *     return PluginBlockFluidClassic.Hooks.place(this, world, pos, fluidStack, doPlace, this.getDefaultState());
         * }
         */
        addMethod(classNode, "place", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;Z)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ILOAD, 4);
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
            generator.visitMethodInsn(INVOKESTATIC, "net/minecraft/block/BlockLiquid", obfuscated ? "func_176361_a" : "getFlowingBlock", "(Lnet/minecraft/block/material/Material;)Lnet/minecraft/block/BlockDynamicLiquid;", false);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false);
            generator.visitMethodInsn(INVOKESTATIC, getFluidClassicClass(), "place", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;ZLnet/minecraft/block/state/IBlockState;)I", false);
        });
        /*
         * drain:
         * New code:
         * //allow the drain method to drain fluidlogged blocks
         * @ASMGenerated
         * public FluidStack drain(World world, BlockPos pos, boolean doDrain)
         * {
         *     return PluginBlockFluidClassic.Hooks.drain(this, world, pos, doDrain, null);
         * }
         */
        addMethod(classNode, "drain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Z)Lnet/minecraftforge/fluids/FluidStack;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitInsn(ACONST_NULL);
            generator.visitMethodInsn(INVOKESTATIC, getFluidClassicClass(), "drain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;", false);
        });
        /*
         * canDrain:
         * New code:
         * //IFluidBlock implementation
         * @ASMGenerated
         * public boolean canDrain(World world, BlockPos pos)
         * {
         *     return PluginBlockFluidClassic.Hooks.canDrain(this, world, pos);
         * }
         */
        addMethod(classNode, "canDrain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, getFluidClassicClass(), "canDrain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false);
        });
        /*
         * getFilledPercentage:
         * New code:
         * //IFluidBlock implementation
         * @ASMGenerated
         * public float getFilledPercentage(World world, BlockPos pos)
         * {
         *     return Hooks.getFilledPercentage(FluidloggedUtils.getFluidOrReal(world, pos), world, pos);
         * }
         */
        addMethod(classNode, "getFilledPercentage", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)F", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getFilledPercentage", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F", false);
        });
        /*
         * getFlowCost:
         * New code:
         * // When lava flows in the overworld, its fluid level increases by 2 instead of 1
         * @ASMGenerated
         * public int getFlowCost(FluidState fluidState, World world)
         * {
         *     return Hooks.getFlowCost(fluidState, world);
         * }
         */
        addMethod(classNode, "getFlowCost", "(Lgit/jbredwards/fluidlogged_api/api/util/FluidState;Lnet/minecraft/world/World;)I",
            "getFlowCost", "(Lgit/jbredwards/fluidlogged_api/api/util/FluidState;Lnet/minecraft/world/World;)I", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * requiresUpdates:
         * New code:
         * // Improve performance when loading chunks
         * @ASMGenerated
         * public boolean requiresUpdates()
         * {
         *     return false;
         * }
         */
        addMethod(classNode, obfuscated ? "func_149698_L" : "requiresUpdates", "()Z", null, null, generator -> generator.visitInsn(ICONST_0));
        return true;
    }

    @Nonnull
    static String getFluidHookClass() { return "git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidBase$Hooks"; }

    @Nonnull
    static String getFluidClassicClass() { return "git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidClassic$Hooks"; }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean checkForMixing(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
            if(state.getMaterial() == Material.LAVA) {
                final int level = FluidState.of(state).getLevel();
                if(level > 4) return false;

                @Nonnull final FluidCache cache = new FluidCache(world, pos, 2, 2);
                if(!cache.getBlockState(pos).getBlock().isReplaceable(cache, pos)) return false;

                for(@Nonnull final EnumFacing side : EnumFacing.VALUES) {
                    if(!FluidloggedAPIConfig.fixBadFluidMixing || FluidloggedUtils.canFluidFlow(cache, pos, cache.getBlockState(pos), side)) {
                        @Nonnull final BlockPos offset = pos.offset(side);
                        if(side != EnumFacing.DOWN && cache.getFluidOrReal(offset).getMaterial() == Material.WATER
                        && (!FluidloggedAPIConfig.fixBadFluidMixing || FluidloggedUtils.canFluidFlow(cache, offset, cache.getBlockState(offset), side.getOpposite()))) {
                            world.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos, pos, (level == 0 ? Blocks.OBSIDIAN : Blocks.COBBLESTONE).getDefaultState()));
                            ((BlockLiquid)state.getBlock()).triggerMixEffects(world, pos);
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        @Nonnull
        public static BlockStateContainer createLiquidBlockState(@Nonnull Block block) {
            return new BlockStateContainer.Builder(block)
                    .add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]))
                    .add(BlockLiquid.LEVEL).build();
        }

        @Nonnull
        public static IBlockState getLiquidExtendedState(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final IBlockState oldState) {
            return FluidExtendedStateHandler.getExtendedState(oldState, new SpecializedFluidNeighborInfo.Vanilla(world, pos, FluidState.of(oldState), 1), info ->
                    FluidFlowHandler.getFlowAngle(((BlockLiquid)info.getOrigin().getBlock()).getFlow(info, pos, info.getOrigin().getState())));
        }

        public static float getBlockLiquidHeight(@Nonnull IBlockState state, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
            final IBlockState up = worldIn.getBlockState(pos.up());
            final boolean flag = FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(worldIn, pos.up(), up).getFluid(), FluidloggedUtils.getFluidFromState(state))
                    && FluidloggedUtils.canFluidFlow(worldIn, pos.up(), up, EnumFacing.DOWN)
                    && FluidloggedUtils.canFluidFlow(worldIn, pos, worldIn.getBlockState(pos), EnumFacing.UP);

            return flag ? 1 : 1 - BlockLiquid.getLiquidHeightPercent(state.getValue(BlockLiquid.LEVEL));
        }

        public static float getFilledPercentage(@Nonnull IBlockState state, @Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos) {
            return Math.min(getBlockLiquidHeight(state, worldIn, pos) * 9f/8, 1); // apply BlockFluidBase inaccuracy for consistency
        }

        @Nonnull
        public static Vec3d getFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
            return FluidFlowHandler.getFlowVec(world instanceof ISpecializedFluidNeighborInfo ? (ISpecializedFluidNeighborInfo)world
                    : new SpecializedFluidNeighborInfo.Vanilla(world, pos, FluidloggedUtils.getFluidState(world, pos, here), 1));
        }

        public static int getFlowCost(@Nonnull final FluidState fluidState, @Nonnull final World world) {
            return fluidState.getMaterial() == Material.LAVA && !world.provider.doesWaterVaporize() ? 2 : 1;
        }

        @Nonnull
        public static Fluid getLiquid(@Nonnull Material material) {
            return material == Material.WATER ? FluidRegistry.WATER : FluidRegistry.LAVA;
        }

        @Nonnull
        public static EnumBlockRenderType getRenderType(@Nonnull EnumBlockRenderType oldType) {
            return FluidloggedAPIConfig.fancyFluidRenderer ? EnumBlockRenderType.MODEL : oldType;
        }

        //helper, exists to fix issue#59
        public static double getSlopeAngle(@Nonnull BlockLiquid block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final Vec3d vec = block.getFlow(world, pos, world.getBlockState(pos));
            return vec.x == 0 && vec.z == 0 ? -1000 : MathHelper.atan2(vec.z, vec.x) - Math.PI / 2;
        }

        public static boolean spawnDripParticles(@Nonnull final IBlockState state, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Random rand) {
            FluidloggedUtils.positionDripParticle(world, pos, FluidState.of(state)).ifPresent(particlePos -> world.spawnParticle(state.getMaterial() == Material.WATER ? EnumParticleTypes.DRIP_WATER : EnumParticleTypes.DRIP_LAVA, particlePos.x, particlePos.y, particlePos.z, 0, 0, 0));
            return false;
        }
    }
}

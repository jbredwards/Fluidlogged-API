/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.IFluidBlock;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.Map;

/**
 * modded fluids work properly with the mod
 * @author jbred
 *
 */
public final class PluginBlockFluidClassic implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidClassic$Accessor");
        addMethod(classNode, "canCreateSource_Public", "()Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z");
        });
        /*
         * Implement IFluidloggableFluid, allows modded fluids that extend this class to be fluidloggable by default
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/fluid/IFluidloggableFluid");
        /*
         * getQuantaValue:
         * New code:
         * //allow this method to be able to get the quanta value from FluidStates
         * @Override
         * public int getQuantaValue(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.getQuantaValue(this, world, pos, quantaPerBlock);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getQuantaValue"),
            "getQuantaValue", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
            }
        );
        /*
         * updateTick
         * New code:
         * //determines how this fluid behaves based on FluidStates
         * @Override
         * public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand)
         * {
         *     Hooks.fluidUpdateTick(world, pos, state, this.displacements);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "fluidUpdateTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Map;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "displacements", "Ljava/util/Map;");
            }
        );
        /*
         * place:
         * New code:
         * //allow the place method to fluidlog blocks
         * @Override
         * public int place(World world, BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace)
         * {
         *     return Hooks.place(this, world, pos, fluidStack, doPlace, this.getDefaultState());
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("place"),
            "place", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;ZLnet/minecraft/block/state/IBlockState;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false);
            }
        );
        /*
         * drain:
         * New code:
         * //allow the drain method to drain fluidlogged blocks
         * @Override
         * @Nullable
         * public FluidStack drain(World world, BlockPos pos, boolean doDrain)
         * {
         *     return Hooks.drain(this, world, pos, doDrain, this.stack);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("drain"),
            "drain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "stack", "Lnet/minecraftforge/fluids/FluidStack;");
            }
        );
        /*
         * canDrain & isSourceBlock:
         * New code:
         * // check FluidState and allow compatible fluids
         * public boolean {canDrain | isSourceBlock}(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.canDrain(this, world, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("isSourceBlock") || method.name.equals("canDrain"),
            "canDrain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void fluidUpdateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull final Map<Block, Boolean> displacements) {
            FluidFlowHandler.updateClassic(world, pos, FluidState.of(state), displacements);
        }

        public static boolean canDrain(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos);
            return FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), block.getFluid()) && fluidState.isSource();
        }

        @Nullable
        public static FluidStack drain(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, boolean doDrain, @Nullable FluidStack stack) {
            @Nonnull final Chunk chunk = world.getChunk(pos);
            doDrain &= !world.isRemote; // prevent bucket desync

            // drain IBlockState
            @Nonnull final IBlockState here = chunk.getBlockState(pos);
            if(FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromState(here), block.getFluid())) {
                if(doDrain) world.setBlockState(pos, Blocks.AIR.getDefaultState());
                return here.getValue(BlockLiquid.LEVEL) > 0 ? null : stack == null ? new FluidStack(block.getFluid(), Fluid.BUCKET_VOLUME) : stack;
            }

            // drain FluidState
            @Nonnull final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
            if(!FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), block.getFluid())) return null;
            else if(doDrain) FluidloggedUtils.setFluidState(world, pos, here, FluidState.EMPTY, false);
            return !fluidState.isSource() ? null : stack == null ? fluidState.createFluidStack() : stack.copy();
        }

        public static int getQuantaValue(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int quantaPerBlock) {
            @Nonnull final FluidCache cache = new FluidCache(world, pos, 0, 1);
            @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(cache, pos);

            return FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), block.getFluid()) ? quantaPerBlock - fluidState.getLevel() : cache.isAirBlock(pos) ? 0 : -1;
        }

        public static int place(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace, @Nonnull IBlockState defaultState) {
            if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
            else if(doPlace && !world.isRemote) {
                @Nonnull final FluidState fluidState = FluidState.of(defaultState);

                @Nonnull final Chunk chunk = world.getChunk(pos);
                @Nonnull final IBlockState here = chunk.getBlockState(pos);

                // check that any existing FluidState is replaceable by the new one
                @Nonnull final FluidState fluidHere = FluidloggedUtils.getFluidState(chunk, pos, here);
                if(fluidHere.getFluid() == fluidState.getFluid() && fluidHere.getLevel() == fluidState.getLevel() || fluidHere.getBlock() instanceof IFluidloggableFluid
                        && !((IFluidloggableFluid)fluidHere.getBlock()).isReplaceableByOther(world, fluidHere, fluidState, true)) return Fluid.BUCKET_VOLUME;

                // if the block here is fluidloggable by the new FluidState, fluidlog the block here
                @Nonnull final IFluidloggableFluid handler = (IFluidloggableFluid)block;
                if(handler.isFluidloggableFluid(fluidState) && handler.isStateFluidloggable(here, world, pos, fluidState) && FluidloggedUtils.setFluidState(world, pos, here, fluidState, true)) return Fluid.BUCKET_VOLUME;

                // if the block here is not fluidloggable by the new FluidState, destroy it and place the FluidState as a block
                FluidUtil.destroyBlockOnFluidPlacement(world, pos);
                world.setBlockState(pos, defaultState, Constants.BlockFlags.DEFAULT_AND_RERENDER);
            }

            return Fluid.BUCKET_VOLUME;
        }
    }

    public interface Accessor
    {
        boolean canCreateSource_Public();
    }
}

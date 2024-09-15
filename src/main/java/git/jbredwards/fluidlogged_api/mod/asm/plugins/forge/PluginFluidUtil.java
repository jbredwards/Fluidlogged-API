/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidFinite;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.IFluidBlock;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.wrappers.FluidBlockWrapper;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * changes some of this class's util functions to be FluidState sensitive
 * @author jbred
 *
 */
public final class PluginFluidUtil implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "tryPickUpFluid", "(Lnet/minecraft/item/ItemStack;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/FluidActionResult;")) return 2;
        return checkMethod(method, "tryPlaceFluid", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/capability/IFluidHandler;Lnet/minecraftforge/fluids/FluidStack;)Z") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * tryPickupFluid: (changes are around line 565)
         * Old code:
         * IBlockState state = worldIn.getBlockState(pos);
         *
         * New code:
         * //check for FluidState before assuming no fluid blocks are at the position
         * IBlockState state = FluidloggedUtils.getFluidOrReal(worldIn, pos);
         */
        if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * tryPlaceFluid: (changes are around line 640)
         * Old code:
         * if (!world.isAirBlock(pos) && !isDestNonSolid && !isDestReplaceable)
         * {
         *     return false;
         * }
         *
         * New code:
         * //if the position can't be fluidlogged and isn't replaceable, return false
         * if (!Hooks.isFluidloggable(world, pos, resource, destBlockState) && !isDestNonSolid && !isDestReplaceable)
         * {
         *     return false;
         * }
         */
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock", null)) {
            final InsnList list = new InsnList();
            //FluidStack local var
            list.add(new VarInsnNode(ALOAD, 4));
            //IBlockState local var
            list.add(new VarInsnNode(ALOAD, 6));
            //add new code
            list.add(genMethodNode("isFluidloggable", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;Lnet/minecraft/block/state/IBlockState;)Z"));
            instructions.insertBefore(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // make getFluidHandler FluidState-sensitive, and prioritize BlockStates and FluidStates
         * @ASMOverwrite
         * public static IFluidHandler getFluidHandler(World world, BlockPos blockPos, @Nullable EnumFacing side)
         * {
         *     return Hooks.getFluidStateHandler(world, blockPos, side);
         * }
         */
        overrideMethod(classNode, method -> checkMethod(method, "getFluidHandler", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/capability/IFluidHandler;"),
            "getFluidStateHandler", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/capability/IFluidHandler;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static IFluidHandler getFluidStateHandler(@Nonnull World world, @Nonnull BlockPos pos, @Nullable EnumFacing side) {
            @Nonnull final FluidCache cache = new FluidCache(world, pos, 0, 0);
            @Nonnull final IBlockState state = cache.getBlockState(pos);

            // check block here
            if(state.getBlock() instanceof IFluidBlock) {
                @Nullable final TileEntity tile = cache.getTileEntity(pos);
                return tile != null && tile.hasCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, side)
                        ? tile.getCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, side)
                        : new FluidBlockWrapper((IFluidBlock)state.getBlock(), world, pos);
            }

            // check fluid here
            @Nonnull final FluidState fluidState = cache.getFluidState(pos);
            if(fluidState.isValid()) return new FluidBlockWrapper(fluidState.getFluidBlock(), world, pos);

            // check tile here
            @Nullable final TileEntity tile = cache.getTileEntity(pos);
            return tile != null && tile.hasCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, side)
                    ? tile.getCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, side)
                    : null;
        }

        public static boolean isFluidloggable(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack resource, @Nonnull IBlockState destBlockState) {
            @Nonnull final Block block = resource.getFluid().getBlock();
            if(block instanceof IFluidloggableFluid) {
                // for non-finite fluid blocks, assume default state
                if(!(block instanceof BlockFluidFinite)) return ((IFluidloggableFluid)block).isFluidloggableFluid(FluidState.of(block))
                        && ((IFluidloggableFluid)block).isStateFluidloggable(destBlockState, world, pos, FluidState.of(block));
                @Nonnull final FluidState newState = PluginBlockFluidFinite.Hooks.getStateForStack((PluginBlockFluidBase.Accessor)block, world, pos, resource, destBlockState).getKey();
                return !newState.isEmpty() && ((IFluidloggableFluid)block).isFluidloggableFluid(newState) && ((IFluidloggableFluid)block).isStateFluidloggable(destBlockState, world, pos, newState);
            }

            return false;
        }
    }
}

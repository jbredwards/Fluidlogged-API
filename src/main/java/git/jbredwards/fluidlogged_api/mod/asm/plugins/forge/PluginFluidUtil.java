/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidFinite;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
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
        if(checkMethod(method, "getFluidHandler", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/capability/IFluidHandler;")) return 1;
        else if(checkMethod(method, "tryPickUpFluid", "(Lnet/minecraft/item/ItemStack;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/FluidActionResult;")) return 2;
        return checkMethod(method, "tryPlaceFluid", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/capability/IFluidHandler;Lnet/minecraftforge/fluids/FluidStack;)Z") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getFluidHandler: (changes are around line 543)
         * Old code:
         * return null;
         *
         * New code:
         * //allows this method to build fluid handlers off of FluidStates
         * return Hooks.getFluidStateHandler(world, blockPos);
         */
        if(index == 1 && insn.getOpcode() == ACONST_NULL) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("getFluidStateHandler", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraftforge/fluids/capability/IFluidHandler;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        /*
         * tryPickupFluid: (changes are around line 565)
         * Old code:
         * IBlockState state = worldIn.getBlockState(pos);
         *
         * New code:
         * //check for FluidState before assuming no fluid blocks are at the position
         * IBlockState state = FluidloggedUtils.getFluidOrReal(worldIn, pos);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
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

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static IFluidHandler getFluidStateHandler(@Nonnull World world, @Nonnull BlockPos pos) {
            final FluidState fluidState = FluidState.get(world, pos);
            return fluidState.isValid() ? new FluidBlockWrapper(fluidState.getFluidBlock(), world, pos) : null;
        }

        public static boolean isFluidloggable(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack resource, @Nonnull IBlockState destBlockState) {
            @Nonnull final Block block = resource.getFluid().getBlock();
            if(block instanceof IFluidloggableFluid) {
                // for non-finite fluid blocks, assume default state
                if(!(block instanceof BlockFluidFinite)) return ((IFluidloggableFluid)block).isFluidloggableFluid(FluidState.of(block))
                        && ((IFluidloggableFluid)block).isStateFluidloggable(destBlockState, world, pos, FluidState.of(block));

                // ========================================================================================================
                // for finite fluid blocks, calculate the new quantity based on the resource amount and the existing amount
                // ========================================================================================================

                if(resource.amount == 0) return false;
                final float quantaAmount = Fluid.BUCKET_VOLUME / ((PluginBlockFluidBase.Accessor)block).getQuantaPerBlockFloat_Public();
                final int quantaPerBlock = ((PluginBlockFluidBase.Accessor)block).getQuantaPerBlock_Public();
                // If the stack contains more available fluid than the full source block,
                // set a source block
                int closest = Fluid.BUCKET_VOLUME;
                int quanta = quantaPerBlock;
                if(resource.amount < closest) {
                    // Figure out maximum level to match stack amount
                    closest = MathHelper.floor(quantaAmount * MathHelper.floor(resource.amount / quantaAmount));
                    quanta = MathHelper.floor(closest / quantaAmount);
                }

                @Nonnull final FluidState existing = FluidloggedUtils.getFluidState(world, pos, destBlockState);
                if(existing.getFluid() == resource.getFluid()) {
                    final int existingQuanta = existing.getLevel() + 1;
                    final int missingQuanta = quantaPerBlock - existingQuanta;
                    closest = Math.min(closest, MathHelper.floor(missingQuanta * quantaAmount));
                    quanta = Math.min(quanta + existingQuanta, quantaPerBlock);
                }

                // If too little (or too much, technically impossible) fluid is to be placed, abort
                if(quanta < 1 || quanta > 16) return false;
                @Nonnull final FluidState newState = FluidState.of(block.getDefaultState()).withLevel(closest);
                return ((IFluidloggableFluid)block).isFluidloggableFluid(newState) && ((IFluidloggableFluid)block).isStateFluidloggable(destBlockState, world, pos, newState);
            }

            return false;
        }
    }
}

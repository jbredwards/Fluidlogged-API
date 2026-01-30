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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.advancements.CriteriaTriggers;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.stats.StatList;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.EnumHand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidActionResult;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.items.ItemHandlerHelper;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * make vanilla buckets use their IFluidHandler when placing/taking fluids
 * @author jbred
 *
 */
public final class PluginItemBucket implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // make vanilla buckets use their IFluidHandler, so they can place FluidStates
         * @ASMOverwrite
         * public ActionResult<ItemStack> onItemRightClick(World worldIn, EntityPlayer playerIn, EnumHand handIn)
         * {
         *     return Hooks.onRightClickBucket(worldIn, playerIn, handIn, this.containedBlock);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"), "onRightClickBucket",
        "(Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/EnumHand;Lnet/minecraft/block/Block;)Lnet/minecraft/util/ActionResult;", generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/item/ItemBucket", obfuscated ? "field_77876_a" : "containedBlock", "Lnet/minecraft/block/Block;");
        });

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static ActionResult<ItemStack> onRightClickBucket(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final EnumHand hand, @Nonnull final Block contained) {
            @Nonnull final ItemStack held = player.getHeldItem(hand);

            @Nonnull final Vec3d eyeVec = player.getPositionEyes(1);
            @Nonnull final Vec3d reachVec = eyeVec.add(player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue()));
            @Nullable final RayTraceResult trace = world.rayTraceBlocks(eyeVec, reachVec, contained == Blocks.AIR, contained != Blocks.AIR, false);

            // handle any possible event overrides
            @Nullable final ActionResult<ItemStack> eventResult = ForgeEventFactory.onBucketUse(player, world, held, trace);
            if(eventResult != null) return eventResult;
            else if(trace == null || trace.typeOfHit != RayTraceResult.Type.BLOCK) return ActionResult.newResult(EnumActionResult.PASS, held);

            // try taking fluid at the block position
            else if(contained == Blocks.AIR) return drainFluid(world, player, held, trace);

            // try placing the fluid at the block position
            else return placeFluid(world, player, contained, held, trace);
        }

        // helper
        @Nonnull
        public static ActionResult<ItemStack> drainFluid(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final ItemStack held, @Nonnull final RayTraceResult trace) {
            if(world.isBlockModifiable(player, trace.getBlockPos())) {
                @Nonnull final FluidActionResult filledResult = FluidUtil.tryPickUpFluid(held, player, world, trace.getBlockPos(), trace.sideHit);
                if(filledResult.isSuccess()) {
                    if(!player.isCreative()) {
                        held.shrink(1);
                        if(held.isEmpty()) return ActionResult.newResult(EnumActionResult.SUCCESS, filledResult.getResult());
                        ItemHandlerHelper.giveItemToPlayer(player, filledResult.getResult());
                    }

                    return ActionResult.newResult(EnumActionResult.SUCCESS, held);
                }
            }

            // could not interact with fluid
            return ActionResult.newResult(EnumActionResult.FAIL, held);
        }

        // helper
        @Nonnull
        public static ActionResult<ItemStack> placeFluid(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final Block contained, @Nonnull final ItemStack held, @Nonnull final RayTraceResult trace) {
            return placeFluid(world, player, contained, held, trace, held.getItem());
        }

        // helper
        @Nonnull
        public static ActionResult<ItemStack> placeFluid(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final Block contained, @Nonnull final ItemStack held, @Nonnull final RayTraceResult trace, @Nonnull final Item itemForStats) {
            @Nullable final Fluid fluid = FluidloggedUtils.getFluidFromBlock(contained);
            if(fluid != null && world.isBlockModifiable(player, trace.getBlockPos())) {
                @Nonnull final BlockPos targetPos = trace.sideHit == EnumFacing.UP
                        && world.getBlockState(trace.getBlockPos()).getBlock().isReplaceable(world, trace.getBlockPos())
                        || FluidloggedAPIConfig.bucketFluidlogging.test(player) && isFluidloggable(world, trace.getBlockPos(), contained)
                        ? trace.getBlockPos() : trace.getBlockPos().offset(trace.sideHit);

                // can the player place there?
                if(world.isBlockModifiable(player, targetPos) && player.canPlayerEdit(targetPos, trace.sideHit, held)) {
                    @Nonnull final FluidActionResult drainedResult = FluidUtil.tryPlaceFluid(null, world, targetPos, held, new FluidStack(fluid, Fluid.BUCKET_VOLUME));

                    // drained fluid from bucket and placed it at the pos
                    if(drainedResult.isSuccess()) {
                        if(player instanceof EntityPlayerMP) CriteriaTriggers.PLACED_BLOCK.trigger((EntityPlayerMP)player, targetPos, held);
                        player.addStat(Objects.requireNonNull(StatList.getObjectUseStats(itemForStats)));

                        if(!player.isCreative()) {
                            held.shrink(1);
                            if(held.isEmpty()) return ActionResult.newResult(EnumActionResult.SUCCESS, drainedResult.getResult());
                            ItemHandlerHelper.giveItemToPlayer(player, drainedResult.getResult());
                        }

                        return ActionResult.newResult(EnumActionResult.SUCCESS, held);
                    }
                }
            }

            // could not interact with fluid
            return ActionResult.newResult(EnumActionResult.FAIL, held);
        }

        // helper
        public static boolean isFluidloggable(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final Block fluid) {
            return fluid instanceof IFluidloggableFluid && ((IFluidloggableFluid)fluid).isFluidloggableFluid(FluidState.of(fluid))
                    && ((IFluidloggableFluid)fluid).isStateFluidloggable(world.getBlockState(pos), world, pos, FluidState.of(fluid));
        }
    }
}

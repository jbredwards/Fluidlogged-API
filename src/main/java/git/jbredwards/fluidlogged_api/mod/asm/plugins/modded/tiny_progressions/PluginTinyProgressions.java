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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tiny_progressions;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.PluginItemBucket;
import net.minecraft.block.BlockCauldron;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.init.SoundEvents;
import net.minecraft.item.ItemStack;
import net.minecraft.stats.StatList;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumHand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * allow tiny progressions' bucket to recognise tanks and FluidStates
 * @author jbred
 *
 */
public final class PluginTinyProgressions implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // allow tiny progressions' bucket to recognise tanks and FluidStates
         * @ASMOverwrite
         * public ActionResult<ItemStack> onItemRightClick(World worldIn, EntityPlayer playerIn, EnumHand handIn)
         * {
         *     return Hooks.onItemRightClick(worldIn, playerIn, handIn, this.containedBlock);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"), "onItemRightClick",
        "(Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/EnumHand;)Lnet/minecraft/util/ActionResult;", generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
        });

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static ActionResult<ItemStack> onRightClickBucket(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final EnumHand hand) {
            @Nonnull final ItemStack held = player.getHeldItem(hand);

            @Nonnull final Vec3d eyeVec = player.getPositionEyes(1);
            @Nonnull final Vec3d reachVec = eyeVec.add(player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue()));
            @Nullable final RayTraceResult trace = world.rayTraceBlocks(eyeVec, reachVec, false, true, false);

            // tiny progressions handles cauldron interactions here
            if(trace != null && trace.typeOfHit == RayTraceResult.Type.BLOCK) {
                @Nonnull final IBlockState state = world.getBlockState(trace.getBlockPos());
                if(state.getBlock() instanceof BlockCauldron) {
                    if(!world.isRemote && state.getValue(BlockCauldron.LEVEL) < 3) {
                        player.addStat(StatList.CAULDRON_FILLED);
                        ((BlockCauldron)state.getBlock()).setWaterLevel(world, trace.getBlockPos(), state, 3);
                        world.playSound(null, trace.getBlockPos(), SoundEvents.ITEM_BUCKET_EMPTY, SoundCategory.BLOCKS, 1, 1);
                    }

                    return ActionResult.newResult(EnumActionResult.SUCCESS, held);
                }
            }

            // handle any possible event overrides
            @Nullable final ActionResult<ItemStack> eventResult = ForgeEventFactory.onBucketUse(player, world, held, trace);
            if(eventResult != null) return eventResult;

            // place water if the player is looking at a block
            else if(trace == null || trace.typeOfHit != RayTraceResult.Type.BLOCK) return ActionResult.newResult(EnumActionResult.PASS, held);
            else return PluginItemBucket.Hooks.placeFluid(world, player, Blocks.FLOWING_WATER, held, trace);
        }
    }
}

/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.reliquary;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.PluginItemBucket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumHand;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make reliquary's chalice use its IFluidHandler when placing/taking fluids
 * @author jbred
 *
 */
public final class PluginReliquary implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // make reliquary's chalice use its IFluidHandler when placing/taking fluids
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
        public static ActionResult<ItemStack> onItemRightClick(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final EnumHand hand) {
            @Nonnull final ItemStack held = player.getHeldItem(hand);
            final boolean enabled = held.hasTagCompound() && held.getTagCompound().getBoolean("enabled");

            @Nonnull final Vec3d eyeVec = player.getPositionEyes(1);
            @Nonnull final Vec3d reachVec = eyeVec.add(player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue()));
            @Nullable final RayTraceResult trace = world.rayTraceBlocks(eyeVec, reachVec, enabled, !enabled, false);

            // players can drink from the chalice
            if(trace == null || trace.typeOfHit != RayTraceResult.Type.BLOCK) {
                if(!enabled) player.setActiveHand(hand);
                return ActionResult.newResult(EnumActionResult.SUCCESS, held);
            }

            // handle any possible event overrides
            @Nullable final ActionResult<ItemStack> eventResult = ForgeEventFactory.onBucketUse(player, world, held, trace);
            if(eventResult != null) return eventResult;

            // chalice fluid interaction
            else return enabled ? PluginItemBucket.Hooks.drainFluid(world, player, held, trace) : PluginItemBucket.Hooks.placeFluid(world, player, Blocks.FLOWING_WATER, held, trace);
        }
    }
}

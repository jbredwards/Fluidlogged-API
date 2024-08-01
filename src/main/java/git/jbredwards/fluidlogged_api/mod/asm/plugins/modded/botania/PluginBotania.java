/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.botania;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumHand;
import net.minecraft.util.EnumParticleTypes;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandler;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * allow botania's void bucket item to recognise tanks and FluidStates
 * @author jbred
 *
 */
public final class PluginBotania implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // allow botania's void bucket item to recognise tanks and FluidStates
         * @ASMOverwrite
         * public ActionResult<ItemStack> onItemRightClick(World world, EntityPlayer player, @Nonnull EnumHand hand)
         * {
         *     return Hooks.onItemRightClick(world, player, hand);
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

            @Nonnull final Vec3d eyeVec = player.getPositionEyes(1);
            @Nonnull final Vec3d reachVec = eyeVec.add(player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue()));
            @Nullable final RayTraceResult trace = world.rayTraceBlocks(eyeVec, reachVec, true);

            // support forge's bucket fill event
            @Nullable final ActionResult<ItemStack> eventResult = ForgeEventFactory.onBucketUse(player, world, held, trace);
            if(eventResult != null) return eventResult;

            // didn't click a block
            else if(trace == null || trace.typeOfHit != RayTraceResult.Type.BLOCK) return ActionResult.newResult(EnumActionResult.PASS, held);
            @Nullable final IFluidHandler handler = FluidUtil.getFluidHandler(world, trace.getBlockPos(), trace.sideHit);
            @Nullable final FluidStack drained = handler != null ? handler.drain(Fluid.BUCKET_VOLUME, true) : null;

            // clicked pos has a fluid, and it can be drained
            if(drained != null && drained.amount > 0) {
                player.playSound(drained.getFluid().getFillSound(drained), 1, 1);
                for(int i = 0; i < 5; i++) world.spawnParticle(EnumParticleTypes.EXPLOSION_NORMAL, trace.getBlockPos().getX() + Math.random(), trace.getBlockPos().getY() + Math.random(), trace.getBlockPos().getZ() + Math.random(), 0, 0, 0);
                return ActionResult.newResult(EnumActionResult.SUCCESS, held);
            }

            return ActionResult.newResult(EnumActionResult.PASS, held);
        }
    }
}

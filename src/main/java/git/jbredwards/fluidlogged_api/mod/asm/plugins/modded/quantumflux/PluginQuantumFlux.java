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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.quantumflux;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.PluginItemBucket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.ItemStack;
import net.minecraft.stats.StatList;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumHand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.FluidTankProperties;
import net.minecraftforge.fluids.capability.IFluidTankProperties;
import net.minecraftforge.fluids.capability.wrappers.FluidBucketWrapper;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * give void buckets an IFluidHandler, and account for FluidStates when draining fluids
 * @author jbred
 *
 */
public final class PluginQuantumFlux implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // make void buckets account for FluidStates
         * @ASMOverwrite
         * public ActionResult<ItemStack> onItemRightClick(World worldIn, EntityPlayer playerIn, EnumHand handIn)
         * {
         *     return Hooks.onItemRightClick(worldIn, playerIn, handIn);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"),
            "onItemRightClick", "(Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/EnumHand;)Lnet/minecraft/util/ActionResult;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        /*
         * New code:
         * // give void buckets an IFluidHandler capability, for better mod compatibility
         * @ASMGenerated
         * public ICapabilityProvider initCapabilities(ItemStack stack, NBTTagCompound nbt)
         * {
         *     return Hooks.getCapabilities(stack);
         * }
         */
        addMethod(classNode, "initCapabilities", "(Lnet/minecraft/item/ItemStack;Lnet/minecraft/nbt/NBTTagCompound;)Lnet/minecraftforge/common/capabilities/ICapabilityProvider;",
            "getCapabilities", "(Lnet/minecraft/item/ItemStack;)Lnet/minecraftforge/common/capabilities/ICapabilityProvider;", generator -> generator.visitVarInsn(ALOAD, 1)
        );
        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks extends FluidBucketWrapper
    {
        @Nonnull
        public static ActionResult<ItemStack> onItemRightClick(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final EnumHand hand) {
            if(player.isSneaking()) return PluginItemBucket.Hooks.onRightClickBucket(world, player, hand, Blocks.AIR);
            @Nonnull final ItemStack held = player.getHeldItem(hand);

            @Nonnull final Vec3d eyeVec = player.getPositionEyes(1);
            @Nonnull final Vec3d reachVec = eyeVec.add(player.getLookVec().scale(player.getEntityAttribute(EntityPlayer.REACH_DISTANCE).getAttributeValue()));
            @Nullable final RayTraceResult trace = world.rayTraceBlocks(eyeVec, reachVec, true);

            // handle any possible event overrides
            @Nullable final ActionResult<ItemStack> eventResult = ForgeEventFactory.onBucketUse(player, world, held, trace);
            if(eventResult != null) return eventResult;
            else if(trace == null || trace.typeOfHit != RayTraceResult.Type.BLOCK) return ActionResult.newResult(EnumActionResult.PASS, held);

            @Nonnull final BlockPos.MutableBlockPos pos = new BlockPos.MutableBlockPos();
            @Nonnull final RayTraceResult drainTrace = new RayTraceResult(trace.hitVec, trace.sideHit, pos);
            @Nonnull FluidState fluidState;

            // mostly copied from QuantumFlux, for identical functionality
            @Nonnull final Set<Fluid> drainedFluids = new HashSet<>();
            final int originX = trace.getBlockPos().getX(), originY = trace.getBlockPos().getY(), originZ = trace.getBlockPos().getZ();
            for(int y = originY - 1; y <= originY; y++) {
                for(int x = -2; x <= 3; x++) {
                    int count = 2;
                    if(x < 0) count = 2 + x;
                    else if(x > 0) count = 2 - x;
                    for(int z = 0; z <= count; z++) {
                        fluidState = FluidloggedUtils.getFluidState(world, pos.setPos(originX + x, y, originZ + z));
                        if(fluidState.isValid() && FluidloggedUtils.setFluidToAir(world, pos, null, 3)) drainedFluids.add(fluidState.getFluid());

                        fluidState = FluidloggedUtils.getFluidState(world, pos.setPos(originX + x, y, originZ - z));
                        if(fluidState.isValid() && FluidloggedUtils.setFluidToAir(world, pos, null, 3)) drainedFluids.add(fluidState.getFluid());
                    }
                }
            }

            if(drainedFluids.isEmpty()) return ActionResult.newResult(EnumActionResult.FAIL, held);
            player.addStat(Objects.requireNonNull(StatList.getObjectUseStats(held.getItem())));
            drainedFluids.forEach(f -> world.playSound(null, player.posX, player.posY + 0.5, player.posZ, f.getFillSound(), SoundCategory.BLOCKS, 1, 1));
            return ActionResult.newResult(EnumActionResult.SUCCESS, held);
        }

        @Nonnull
        public static ICapabilityProvider getCapabilities(@Nonnull final ItemStack stack) { return new Hooks(stack); }
        public Hooks(@Nonnull final ItemStack container) { super(container); }

        @Nonnull
        @Override
        public IFluidTankProperties[] getTankProperties() {
            return new IFluidTankProperties[] {new FluidTankProperties(null, Integer.MAX_VALUE, true, false)};
        }

        @Nullable
        @Override
        public FluidStack drain(final int maxDrain, final boolean doDrain) { return null; }

        @Nullable
        @Override
        public FluidStack drain(@Nullable final FluidStack resource, final boolean doDrain) { return null; }

        @Override
        public int fill(@Nullable final FluidStack resource, final boolean doFill) { return resource != null && resource.amount > 0 ? resource.amount : 0; }
    }
}

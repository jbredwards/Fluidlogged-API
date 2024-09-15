/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.mekanism;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.PluginItemBucket;
import mekanism.common.block.states.BlockStateMachine;
import mekanism.common.item.ItemBlockMachine;
import mekanism.common.util.MekanismUtils;
import mekanism.common.util.SecurityUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumHand;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make mekanism's fluid tank use its IFluidHandler when placing/taking fluids
 * @author jbred
 *
 */
public final class PluginMekanismTank implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // make mekanism's fluid tank use its IFluidHandler when placing/taking fluids
         * @ASMOverwrite
         * public ActionResult<ItemStack> onItemRightClick(World worldIn, EntityPlayer playerIn, EnumHand handIn)
         * {
         *     return Hooks.onItemRightClick(worldIn, playerIn, handIn, this.containedBlock);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"), "onItemRightClick",
        "(Lmekanism/common/item/ItemBlockMachine;Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/EnumHand;)Lnet/minecraft/util/ActionResult;", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static ActionResult<ItemStack> onItemRightClick(@Nonnull final ItemBlockMachine machine, @Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final EnumHand hand) {
            @Nonnull final ItemStack held = player.getHeldItem(hand);
            switch(BlockStateMachine.MachineType.get(held)) {
                case PERSONAL_CHEST: {
                    if(!world.isRemote) {
                        if(machine.getOwnerUUID(held) == null) machine.setOwnerUUID(held, player.getUniqueID());
                        if(SecurityUtils.canAccess(player, held)) MekanismUtils.openItemGui(player, hand, 19);
                        else SecurityUtils.displayNoAccess(player);
                    }

                    break;
                }

                case FLUID_TANK: {
                    if(machine.getBucketMode(held)) {
                        if(SecurityUtils.canAccess(player, held)) {
                            // drain fluid here
                            if(!player.isSneaking()) return PluginItemBucket.Hooks.onRightClickBucket(world, player, hand, Blocks.AIR);

                            // place contained fluid
                            @Nullable final FluidStack contained = machine.getFluid(held);
                            if(contained == null || contained.amount < Fluid.BUCKET_VOLUME || !contained.getFluid().canBePlacedInWorld())
                                return ActionResult.newResult(EnumActionResult.FAIL, held);
                            else return PluginItemBucket.Hooks.onRightClickBucket(world, player, hand, contained.getFluid().getBlock());
                        }

                        else SecurityUtils.displayNoAccess(player);
                    }

                    break;
                }
            }

            return ActionResult.newResult(EnumActionResult.PASS, held);
        }
    }
}

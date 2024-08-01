/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.nethercraft;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.PluginItemBucket;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.EnumHand;
import net.minecraft.util.NonNullList;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * make nethercraft's bucket use its IFluidHandler when placing/taking fluids
 * @author jbred
 *
 */
public final class PluginNethercraftClassic implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        // fluid capability
        if(classNode.name.endsWith("Wrapper")) {
            // Accessor
            classNode.interfaces.add(getAccessorClass());
            addMethod(classNode, "setContainer_Public", "(Lnet/minecraft/item/ItemStack;)V", null, null, generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitFieldInsn(PUTFIELD, "net/minecraftforge/fluids/capability/wrappers/FluidBucketWrapper", "container", "Lnet/minecraft/item/ItemStack;");
            });
            /*
             * New code:
             * // Call nethercraft's (unused) setFluid method
             * @ASMOverwrite
             * public void setFluid(@Nullable FluidStack fluid)
             * {
             *     Hooks.setFluid(this, fluid);
             * }
             */
            addMethod(classNode, "setFluid", "(Lnet/minecraftforge/fluids/FluidStack;)V", "setFluid", withAccessorClass("(L%s;Lnet/minecraftforge/fluids/FluidStack;)V"), generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
            });
        }
        // bucket item
        else {
            /*
             * New code:
             * // add lava bucket to creative tab
             * @ASMGenerated
             * public void getSubItems(CreativeTabs tab, NonNullList<ItemStack> items)
             * {
             *     Hooks.getSubItems(this, tab, items);
             * }
             */
            addMethod(classNode, obfuscated ? "func_150895_a" : "getSubItems", "(Lnet/minecraft/creativetab/CreativeTabs;Lnet/minecraft/util/NonNullList;)V", "(Lnet/minecraft/creativetab/CreativeTabs;Lnet/minecraft/util/NonNullList<Lnet/minecraft/item/ItemStack;>;)V",
                "getSubItems", "(Lnet/minecraft/item/Item;Lnet/minecraft/creativetab/CreativeTabs;Lnet/minecraft/util/NonNullList;)V", generator -> {
                    generator.visitVarInsn(ALOAD, 0);
                    generator.visitVarInsn(ALOAD, 1);
                    generator.visitVarInsn(ALOAD, 2);
                }
            );
            /*
             * New code:
             * // make nethercraft's bucket use its IFluidHandler when placing/taking fluids
             * @ASMOverwrite
             * public ActionResult<ItemStack> onItemRightClick(World worldIn, EntityPlayer playerIn, EnumHand handIn)
             * {
             *     return Hooks.onItemRightClick(worldIn, playerIn, handIn, this.containedBlock);
             * }
             */
            overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"),
                "onItemRightClick", "(Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/EnumHand;)Lnet/minecraft/util/ActionResult;", generator -> {
                    generator.visitVarInsn(ALOAD, 1);
                    generator.visitVarInsn(ALOAD, 2);
                    generator.visitVarInsn(ALOAD, 3);
                }
            );
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void getSubItems(@Nonnull final Item item, @Nonnull final CreativeTabs tab, @Nonnull final NonNullList<ItemStack> items) {
            if(item.isInCreativeTab(tab)) {
                items.add(new ItemStack(item, 1, 0));
                items.add(new ItemStack(item, 1, 1));
            }
        }

        @Nonnull
        public static ActionResult<ItemStack> onItemRightClick(@Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final EnumHand hand) {
            return PluginItemBucket.Hooks.onRightClickBucket(world, player, hand, player.getHeldItem(hand).getMetadata() == 0 ? Blocks.AIR : Blocks.FLOWING_LAVA);
        }

        public static void setFluid(@Nonnull final Accessor wrapper, @Nullable final FluidStack stack) {
            wrapper.setContainer_Public(new ItemStack(wrapper.getContainer().getItem(), 1, stack == null ? 0 : 1));
        }
    }

    public interface Accessor extends IFluidHandlerItem
    {
        void setContainer_Public(@Nonnull final ItemStack container);
    }
}

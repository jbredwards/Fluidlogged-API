/*
 * Copyright (c) 2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBucket;
import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fixes modded ItemBucket item instances
 * @author jbred
 *
 */
public final class PluginFluidBucketWrapper implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        return "getFluid".equals(method.name) ? 1 : checkMethod(method, "setFluid", "(Lnet/minecraftforge/fluids/FluidStack;)V") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * getFluid: (changes are around line 95)
         * Old code:
         * return null;
         *
         * New code:
         * // fix modded ItemBucket item instances by using their ItemBucket.containedBlock
         * return Hooks.getFluid(item, null);
         */
        if(index == 1 && insn.getOpcode() == ACONST_NULL) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insert(insn, genMethodNode("getFluid", "(Lnet/minecraft/item/Item;Lnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;"));
            return true;
        }
        /*
         * setFluid: (changes are around line 110)
         * Old code:
         * container = new ItemStack(Items.BUCKET);
         *
         * New code:
         * // fix modded ItemBucket instances by using their container's getContainerItem()
         * container = Hooks.getContainer(new ItemStack(Items.BUCKET), this.container);
         */
        else if(index == 2 && checkField(insn, "container")) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/capability/wrappers/FluidBucketWrapper", "container", "Lnet/minecraft/item/ItemStack;"));
            instructions.insertBefore(insn, genMethodNode("getContainer", "(Lnet/minecraft/item/ItemStack;Lnet/minecraft/item/ItemStack;)Lnet/minecraft/item/ItemStack;"));
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static ItemStack getContainer(@Nonnull final ItemStack fallback, @Nonnull final ItemStack container) {
            return container.getItem().hasContainerItem(container) ? container.getItem().getContainerItem(container).copy() : fallback;
        }

        @Nullable
        public static FluidStack getFluid(@Nonnull final Item bucket, @Nullable final FluidStack fallback) {
            if(!(bucket instanceof ItemBucket)) return fallback;

            @Nullable final Fluid containedFluid = FluidloggedUtils.getFluidFromBlock(((ItemBucket)bucket).containedBlock);
            return containedFluid == null ? fallback : new FluidStack(containedFluid, Fluid.BUCKET_VOLUME);
        }
    }
}

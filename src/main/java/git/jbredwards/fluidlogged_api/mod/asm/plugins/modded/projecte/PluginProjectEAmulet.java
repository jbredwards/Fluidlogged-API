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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.projecte;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item.PluginItemBucket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

import javax.annotation.Nonnull;

/**
 * allow ProjectE's amulets to fluidlog blocks, and to recognize FluidStates when applying a speed boost
 * @author jbred
 *
 */
public final class PluginProjectEAmulet implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_180614_a" : "onItemUse")) return 1;
        else return method.name.equals(obfuscated ? "func_77663_a" : "onUpdate") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        if(index == 1) {
            /*
             * onItemUse:
             * Old code:
             * this.placeWater(world, player, pos.offset(sideHit), hand);
             *
             * New code:
             * // allow the Evertide Amulet to waterlog blocks
             * Hooks.placeWater(this, world, player, pos, sideHit);
             */
            if(checkMethod(insn, "placeWater")) {
                removeFrom(instructions, insn.getPrevious(), -1);
                instructions.insertBefore(insn, genMethodNode("placeWater", "(Lnet/minecraft/item/Item;Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)V"));
                instructions.remove(insn);
                return true;
            }
            /*
             * onItemUse:
             * Old code:
             * this.placeLava(player, pos.offset(sideHit), hand);
             *
             * New code:
             * // allow the Volcanite Amulet to lavalog blocks
             * Hooks.placeLava(this, player, pos, sideHit, world);
             */
            else if(checkMethod(insn, "placeLava")) {
                removeFrom(instructions, insn.getPrevious(), -1);
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
                instructions.insertBefore(insn, genMethodNode("placeLava", "(Lnet/minecraft/item/Item;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraft/world/World;)V"));
                instructions.remove(insn);
                return true;
            }
        }
        /*
         * onUpdate:
         * Old code:
         * if ((world.getBlockState(pos.down()).getBlock() == ... || world.getBlockState(pos.down()).getBlock() == ...) && ...)
         * {
         *     ...
         * }
         *
         * New code:
         * // account for FluidStates
         * if ((FluidloggedUtils.getFluidOrReal(world, pos.down()).getBlock() == ... || FluidloggedUtils.getFluidOrReal(world, pos.down()).getBlock() == ...) && ...)
         * {
         *     ...
         * }
         */
        else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void placeLava(@Nonnull final Item item, @Nonnull final EntityPlayer player, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side, @Nonnull final World world) {
            PluginItemBucket.Hooks.placeFluid(world, player, Blocks.FLOWING_LAVA, new ItemStack(Items.LAVA_BUCKET), new RayTraceResult(Vec3d.ZERO, side, pos), item);
        }

        public static void placeWater(@Nonnull final Item item, @Nonnull final World world, @Nonnull final EntityPlayer player, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side) {
            PluginItemBucket.Hooks.placeFluid(world, player, Blocks.FLOWING_WATER, new ItemStack(Items.WATER_BUCKET), new RayTraceResult(Vec3d.ZERO, side, pos), item);
        }
    }
}

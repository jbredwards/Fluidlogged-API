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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumParticleTypes;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * makes barriers fluidloggable by default
 * @author jbred
 *
 */
public final class PluginBlockBarrier implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * randomDisplayTick:
         * New code:
         * //PluginWorldClient removes the hardcoded barrier particle spawning system, this reimplements it
         * @ASMGenerated
         * @SideOnly(Side.CLIENT)
         * public void randomDisplayTick(IBlockState state, World world, BlockPos pos, Random rand)
         * {
         *     Hooks.fixBarrierParticles(state, world, pos);
         * }
         */
        addMethod(classNode, obfuscated ? "func_180655_c" : "randomDisplayTick", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Ljava/util/Random;)V",
            "fixBarrierParticles", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V", generator -> {
                //adds the client-side only annotation
                generator.visitAnnotation("Lnet/minecraftforge/fml/relauncher/SideOnly;", true)
                        .visitEnum("value", "Lnet/minecraftforge/fml/relauncher/Side;", "CLIENT");
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @SideOnly(Side.CLIENT)
        public static void fixBarrierParticles(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos) {
            final EntityPlayer player = Minecraft.getMinecraft().player;
            if(player.isCreative() && state.getBlock() == Block.getBlockFromItem(player.getHeldItemMainhand().getItem()))
                world.spawnParticle(EnumParticleTypes.BARRIER, pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 0, 0, 0);
        }
    }
}

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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import hellfirepvp.astralsorcery.common.block.fluid.FluidBlockLiquidStarlight;
import hellfirepvp.astralsorcery.common.entities.EntityStarlightReacttant;
import hellfirepvp.astralsorcery.common.item.crystal.base.ItemRockCrystalBase;
import hellfirepvp.astralsorcery.common.util.EntityUtils;
import hellfirepvp.astralsorcery.common.util.ItemUtils;
import hellfirepvp.astralsorcery.common.util.OreDictAlias;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * make astral sorcery's starlight reactants FluidState-sensitive
 * @author jbred
 *
 */
public final class PluginEntityCrystal implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("increaseSize"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * increaseSize:
         * Old code:
         * this.world.setBlockToAir(this.getPosition());
         *
         * New code:
         * // account for FluidStates
         * FluidloggedUtils.setFluidToAir(this.world, this.getPosition(), null, 3);
         */
        if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            instructions.insertBefore(insn, new InsnNode(ACONST_NULL));
            instructions.insertBefore(insn, new InsnNode(ICONST_3));
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "setFluidToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if(classNode.name.equals("hellfirepvp/astralsorcery/common/entities/EntityItemStardust")) {
            overrideMethod(classNode, method -> method.name.equals("canCraft"), "canCraft", "(Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/AxisAlignedBB;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETSTATIC, classNode.name, "boxCraft", "Lnet/minecraft/util/math/AxisAlignedBB;");
            });
        }

        overrideMethod(classNode, method -> method.name.equals("getCraftMode"), "getCraftMode", "(Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/AxisAlignedBB;)I", generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETSTATIC, classNode.name, "boxCraft", "Lnet/minecraft/util/math/AxisAlignedBB;");
        });

        overrideMethod(classNode, method -> method.name.equals("isInLiquidStarlight"), "isInLiquidStarlight", "(Lnet/minecraft/entity/Entity;)Z", generator -> generator.visitVarInsn(ALOAD, 1));
        return classNode.methods.stream().anyMatch(method -> isMethodValid(method, obfuscated));
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canCraft(@Nonnull final Entity crystal, @Nonnull final AxisAlignedBB boxCraft) {
            if(!((EntityStarlightReacttant)crystal).isInLiquidStarlight(crystal)) return false;
            @Nonnull final BlockPos pos = new BlockPos(crystal.posX, crystal.posY, crystal.posZ);

            if(!crystal.world.getBlockState(pos).getBlock().isReplaceable(crystal.world, pos)) return false; // Don't allow the replacing of non-replaceable blocks.
            else return !crystal.world.getEntitiesInAABBexcluding(crystal, boxCraft.offset(crystal.posX, crystal.posY, crystal.posZ), EntityUtils.selectItemClassInstaceof(ItemRockCrystalBase.class)).isEmpty();
        }

        public static int getCraftMode(@Nonnull final Entity crystal, @Nonnull final AxisAlignedBB boxCraft) {
            if(!((EntityStarlightReacttant)crystal).isInLiquidStarlight(crystal)) return -1;
            @Nonnull final BlockPos pos = new BlockPos(crystal.posX, crystal.posY, crystal.posZ);
            @Nonnull final List<Entity> foundEntities = crystal.world.getEntitiesInAABBexcluding(crystal, boxCraft.offset(pos), entity -> true);

            if(foundEntities.isEmpty()) return 0;
            else if(!crystal.world.getBlockState(pos).getBlock().isReplaceable(crystal.world, pos)) return -1; // Don't allow the replacing of non-replaceable blocks.
            else return foundEntities.stream().filter(EntityUtils.selectItemStack(stack -> ItemUtils.hasOreName(stack, OreDictAlias.ITEM_GLOWSTONE_DUST))).count() == 1 ? 1 : -1;
        }

        public static boolean isInLiquidStarlight(@Nonnull final Entity crystal) {
            @Nonnull final BlockPos pos = new BlockPos(crystal.posX, crystal.posY, crystal.posZ);
            @Nonnull final Chunk chunk = crystal.world.getChunk(pos);
            // check for liquid starlight
            @Nonnull final IBlockState state = chunk.getBlockState(pos);
            @Nonnull final FluidState fluidState = FluidloggedUtils.getFluidState(chunk, pos, state);
            return fluidState.getBlock() instanceof FluidBlockLiquidStarlight && fluidState.isSource()
                    && (state.isSideSolid(chunk.getWorld(), pos, EnumFacing.DOWN)
                    || chunk.getBlockState(pos.down()).isSideSolid(chunk.getWorld(), pos.down(), EnumFacing.UP));
        }
    }
}

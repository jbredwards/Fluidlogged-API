/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.astral_sorcery;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import hellfirepvp.astralsorcery.common.block.fluid.FluidBlockLiquidStarlight;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.oredict.OreDictionary;
import org.apache.commons.lang3.ArrayUtils;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.stream.IntStream;

/**
 * make astral sorcery's crystal growth FluidState-sensitive
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
        overrideMethod(classNode, method -> method.name.equals("canCraft"), "canCraft", "(Lnet/minecraft/entity/Entity;)I", generator -> generator.visitVarInsn(ALOAD, 0));
        overrideMethod(classNode, method -> method.name.equals("getCraftMode"), "getCraftMode", "(Lnet/minecraft/entity/Entity;)I", generator -> generator.visitVarInsn(ALOAD, 0));
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canCraft(@Nonnull final Entity crystal) {
            @Nonnull final BlockPos pos = new BlockPos(crystal.posX, crystal.posY, crystal.posZ);
            // check for liquid starlight
            @Nonnull final Chunk chunk = crystal.world.getChunk(pos);
            if(!(FluidloggedUtils.getFluidOrReal(chunk, pos).getBlock() instanceof FluidBlockLiquidStarlight)) return false;
            // check for colliding entities
            @Nonnull final AxisAlignedBB bb = new AxisAlignedBB(pos);
            return IntStream.rangeClosed(
                        MathHelper.clamp(MathHelper.floor((bb.minY - World.MAX_ENTITY_RADIUS) / 16), 0, chunk.getEntityLists().length),
                        MathHelper.clamp(MathHelper.floor((bb.maxY + World.MAX_ENTITY_RADIUS) / 16), 0, chunk.getEntityLists().length)
                    )
                    .mapToObj(i -> chunk.getEntityLists()[i])
                    .flatMap(Collection::stream)
                    .noneMatch(e -> e != crystal && bb.intersects(e.getEntityBoundingBox()));
        }

        public static int getCraftMode(@Nonnull final Entity crystal) {
            @Nonnull final BlockPos pos = new BlockPos(crystal.posX, crystal.posY, crystal.posZ);
            @Nonnull final Chunk chunk = crystal.world.getChunk(pos);
            // check colliding entities for crystal+glowstone recipe
            boolean foundGlowstone = false;
            @Nonnull final AxisAlignedBB bb = new AxisAlignedBB(pos);
            for(@Nonnull final Entity entity : IntStream.rangeClosed(
                    MathHelper.clamp(MathHelper.floor((bb.minY - World.MAX_ENTITY_RADIUS) / 16), 0, chunk.getEntityLists().length),
                    MathHelper.clamp(MathHelper.floor((bb.maxY + World.MAX_ENTITY_RADIUS) / 16), 0, chunk.getEntityLists().length)
            ).mapToObj(i -> chunk.getEntityLists()[i]).flatMap(Collection::stream).toArray(Entity[]::new)) {
                if(entity != crystal && entity.getEntityBoundingBox().intersects(bb)) {
                    if(!foundGlowstone && entity instanceof EntityItem // colliding entity found, check if it's a glowstone item
                    && ArrayUtils.contains(OreDictionary.getOreIDs(((EntityItem)entity).getItem()), OreDictionary.getOreID("dustGlowstone")))
                        foundGlowstone = true;
                    else return -1;
                }
            }
            // crystal+glowstone recipe shouldn't check for FluidStates, it replaces the block here
            if(foundGlowstone) return chunk.getBlockState(pos).getBlock() instanceof FluidBlockLiquidStarlight ? 1 : -1;
            // check crystal growth conditions (if no colliding entities)
            return FluidloggedUtils.getFluidOrReal(chunk, pos).getBlock() instanceof FluidBlockLiquidStarlight ? 0 : -1;
        }
    }
}

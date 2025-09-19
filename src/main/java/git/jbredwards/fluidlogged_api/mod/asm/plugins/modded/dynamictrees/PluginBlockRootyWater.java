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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.dynamictrees;

import com.ferreusveritas.dynamictrees.blocks.BlockRootyWater;
import com.ferreusveritas.dynamictrees.blocks.MimicProperty;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidRegistry;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * make DynamicTrees' water root block use actual fluidlogging, instead of pseudo fluidlogging
 * @author jbred
 *
 */
public final class PluginBlockRootyWater implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return checkMethod(method, "<init>", "(ZLjava/lang/String;)V"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * super(name, Material.WATER, isTileEntity);
         *
         * New code:
         * // Use different material, to let this block be fluidloggable
         * super(name, Material.PLANTS, isTileEntity);
         */
        if(checkField(insn, obfuscated ? "field_151586_h" : "WATER")) {
            ((FieldInsnNode)insn).name = obfuscated ? "field_151585_k" : "PLANTS";
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.methods.removeIf(method
                -> method.name.equals("canRenderInLayer")
                || method.name.equals("doesSideBlockRendering")
                || method.name.equals("getFogColor")
                || method.name.equals(obfuscated ? "func_185484_c" : "getPackedLightmapCoords")
                || method.name.equals(obfuscated ? "func_180657_a" : "harvestBlock")
                || method.name.equals(obfuscated ? "func_176197_a" : "modifyAcceleration")
                || method.name.equals(obfuscated ? "func_189540_a" : "neighborChanged")
                || method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"));
        /*
         * New code:
         * // Don't use a custom block state container class
         * @ASMOverwrite
         * protected BlockStateContainer createBlockState()
         * {
         *     return Hooks.createBlockState(this);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180661_e" : "createBlockState"),
                "createBlockState", "(Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/BlockStateContainer;", generator -> generator.visitVarInsn(ALOAD, 0));
        /*
         * New code:
         * // Don't calculate unused properties
         * @ASMOverwrite
         * public IBlockState getExtendedState(IBlockState state, IBlockAccess access, BlockPos pos)
         * {
         *     return state;
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getExtendedState"),
                null, null, generator -> generator.visitVarInsn(ALOAD, 1));
        /*
         * New code:
         * // Render in cutout layer
         * @ASMOverwrite
         * public BlockRenderLayer getRenderLayer()
         * {
         *     return BlockRenderLayer.CUTOUT_MIPPED;
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180664_k" : "getRenderLayer"),
                null, null, generator -> generator.visitFieldInsn(GETSTATIC, "net/minecraft/util/BlockRenderLayer", "CUTOUT_MIPPED", "Lnet/minecraft/util/BlockRenderLayer;"));
        /*
         * New code:
         * // Use normal particle spawning logic
         * @ASMOverwrite
         * @SideOnly(Side.CLIENT)
         * public boolean addDestroyEffects(World world, BlockPos pos, ParticleManager manager)
         * {
         *     return false;
         * }
         */
        /*
         * New code:
         * // Use normal particle spawning logic
         * @ASMOverwrite
         * @SideOnly(Side.CLIENT)
         * public boolean addHitEffects(IBlockState state, World world, RayTraceResult target, ParticleManager manager)
         * {
         *     return false;
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("addDestroyEffects") || method.name.equals("addHitEffects"),
                null, null, generator -> generator.visitInsn(ICONST_0));
        /*
         * New code:
         * // Add water during world gen
         * @ASMGenerated
         * public void onBlockAdded(World worldIn, BlockPos pos, IBlockState state)
         * {
         *     Hooks.onBlockAdded(worldIn, pos, state);
         * }
         */
        addMethod(classNode, obfuscated ? "func_176213_c" : "onBlockAdded", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)V",
            "onBlockAdded", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        // make water roots override the "applyDefaults" setting, as it must be fluidloggable for its own functionality
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        addMethod(classNode, "overrideApplyDefaultsSetting", "()Z", null, null, generator -> generator.visitInsn(ICONST_1));
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static BlockStateContainer createBlockState(@Nonnull final Block block) {
            return new BlockStateContainer.Builder(block).add(BlockRootyWater.LIFE).add(MimicProperty.MIMIC).add(BlockRootyWater.CORNER_HEIGHTS).add(BlockRootyWater.RENDER_SIDES).build();
        }

        public static void onBlockAdded(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state) {
            if(!world.provider.doesWaterVaporize()) FluidloggedUtils.setFluidState(world, pos, state, FluidState.of(FluidRegistry.WATER), false);
        }
    }
}

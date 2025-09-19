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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cofhcore.PluginCoFHCore;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.ISpecializedFluidNeighborInfo;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl.SpecializedFluidNeighborInfo;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Map;

/**
 * fix conflicts
 * @author jbred
 *
 */
public final class PluginThermalFoundation implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if("interactWithBlock".equals(method.name)) return 1;
        else return method.name.equals(obfuscated ? "func_180650_b" : "updateTick") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final ClassNode classNode, @Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        // interactWithBlock:
        if(index == 1) {
            /*
             * Old code:
             * IBlockState state = world.getBlockState(pos);
             *
             * New code:
             * // Account for FluidStates
             * IBlockState state = FluidloggedUtils.getFluidOrReal(world, pos);
             */
            if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
                // instructions.insert(insn, new TypeInsnNode(CHECKCAST, "net/minecraft/block/state/IBlockState"));
                // instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "java/lang/ThreadLocal", "get", "()java/lang/Object;", false));
                // instructions.insert(insn, new FieldInsnNode(GETSTATIC, "git/jbredwards/fluidlogged_api/mod/asm/plugins/modded/PluginCoFHCore$Hooks", "interact_check_state", "Ljava/lang/ThreadLocal;"));
                // removeFrom(instructions, insn, -2);
            }
            /*
             * Old code:
             * if (state.getBlock() == this)
             * {
             *     ...
             * }
             *
             * New code:
             * // Use a compatible fluid check instead of checking for an exact block
             * if (Hooks.areFluidsCompatible(state.getBlock(), this))
             * {
             *     ...
             * }
             */
            // else if(insn.getOpcode() == IF_ACMPNE && insn.getPrevious().getOpcode() == ALOAD && ((VarInsnNode)insn.getPrevious()).var == 0) {
            //     instructions.insertBefore(insn, genMethodNode("areFluidsCompatible", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;)Z"));
            //     ((JumpInsnNode)insn).setOpcode(IFNE);
            // }
            /*
             * Old code:
             * if (state.isSideSolid(world, pos, ...))
             * {
             *     ...
             * }
             *
             * New code:
             * // The "state" local var has been changed to represent a FluidState block state, so the state here needs to be gathered from the world
             * if (Hooks.isHereSideSolid(state, world, pos, ...))
             * {
             *     ...
             * }
             */
            else if(checkMethod(insn, "isSideSolid", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z")) {
                instructions.insert(insn, genMethodNode("isHereSideSolid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
                instructions.remove(insn);
            }
            /*
             * Old code:
             * if (state.getBlock().isFlammable(world, pos, EnumFacing.UP))
             * {
             *     ...
             * }
             *
             * New code:
             * // Use fire logic
             * if (Hooks.canCatchFire(state.getBlock(), world, pos, EnumFacing.UP))
             * {
             *     ...
             * }
             */
            else if(checkMethod(insn, "isFlammable")) {
                instructions.insert(insn, genMethodNode("canCatchFire", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
                instructions.remove(insn);
            }
            /*
             * Old code:
             * world.setBlockState(...);
             *
             * New code:
             * // Call forge's FluidPlaceBlockEvent event
             * Hooks.setBlockFromFluid(world, ...);
             */
            else if(checkMethod(insn, obfuscated ? "func_175656_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z")) {
                instructions.insert(insn, genMethodNode("setBlockFromFluid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
                instructions.insert(insn, new InsnNode(ICONST_3));
                instructions.remove(insn);
            }
            else if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z")) {
                instructions.insert(insn, genMethodNode("setBlockFromFluid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
                instructions.remove(insn);
            }
        }

        // updateTick:
        else if(index == 2) {
            /*
             * Old code:
             * world.setBlockToAir(pos);
             *
             * New code:
             * // Remove the FluidState
             * FluidloggedUtils.setFluidToAir(world, pos, null, 3);
             */
            // if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            //     instructions.insertBefore(insn, new InsnNode(ACONST_NULL));
            //     instructions.insertBefore(insn, new InsnNode(ICONST_3));
            //     instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "setFluidToAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));
            //     instructions.remove(insn);
            // }
            /*
             * Old code:
             * if (state.getBlock().isFlammable(world, pos, EnumFacing.UP))
             * {
             *     ...
             * }
             *
             * New code:
             * // Use fire logic and fluidlogged api configs
             * if (Hooks.canSpreadFire(state.getBlock(), world, pos, EnumFacing.UP, FluidState.of(state), this.displacements))
             * {
             *     ...
             * }
             */
            // else if(checkMethod(insn, "isFlammable")) {
            //     instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
            //     instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidState", "of", "(Lnet/minecraft/block/state/IBlockState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            //     instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            //     instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "displacements", "Ljava/util/Map;"));
            //     instructions.insertBefore(insn, genMethodNode("canSpreadFire", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;Ljava/util/Map;)Z"));
            //     instructions.remove(insn);
            // }
            /*
             * updateTick:
             * Old code:
             * if (enableSourceFall &&  == 0)
             * {
             *     ...
             * }
             *
             * New code:
             * // Disable thermal's source falling logic, it is reimplemented below
             * if (enableSourceFall && Hooks.falsify(getMetaFromState(state)) == 0)
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, obfuscated ? "func_176201_c" : "getMetaFromState")) instructions.insert(insn, genMethodNode("falsify", "(I)I"));
            /*
             * Old code:
             * super.updateTick(world, pos, state, rand);
             *
             * New code:
             * // Reimplement source falling
             * if(Hooks.sourceFall(world, pos, state, this.enableSourceFall)) return;
             * super.updateTick(world, pos, state, rand);
             */
            else if(checkMethod(getNext(insn, 5), obfuscated ? "func_180650_b" : "updateTick")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
                instructions.insertBefore(insn, new FieldInsnNode(GETSTATIC, classNode.name, "enableSourceFall", "Z"));
                instructions.insertBefore(insn, genMethodNode("sourceFall", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Z)Z"));

                @Nonnull final LabelNode label = new LabelNode();
                instructions.insertBefore(insn, new JumpInsnNode(IFEQ, label));
                instructions.insertBefore(insn, new InsnNode(RETURN));
                instructions.insertBefore(insn, label);
                instructions.insertBefore(insn, new FrameNode(F_SAME, 0, null, 0, null));
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.methods.removeIf(method -> method.name.equals("isEntityInsideMaterial"));
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean areFluidsCompatible(@Nonnull final Block block1, @Nonnull final Block block2) {
            return FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromBlock(block1), FluidloggedUtils.getFluidFromBlock(block2));
        }

        public static boolean canCatchFire(@Nonnull final Block unused, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side) {
            return world.getGameRules().getBoolean("doFireTick") && Blocks.FIRE.canCatchFire(world, pos, side);
        }

        public static boolean canSpreadFire(@Nonnull final Block unused, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side, @Nonnull final FluidState state) {
            @Nonnull final ISpecializedFluidNeighborInfo info = new SpecializedFluidNeighborInfo.Forge(world, pos, state, 1);
            return info.canFlowInto(0, -side.getYOffset(), 0, state.getMetadata(), side, false, false) && info.isVaporizable(0, 0, 0, state, side);
        }

        public static boolean isHereSideSolid(@Nonnull final IBlockState unused, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final EnumFacing side) {
            return world.getBlockState(pos).isSideSolid(world, pos, side);
        }

        public static boolean setBlockFromFluid(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, final int blockFlags) {
            return world.setBlockState(pos, ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos, PluginCoFHCore.Hooks.fluid_pos_access.get(), state), blockFlags);
        }

        public static boolean sourceFall(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, final boolean enabled) {
            if(enabled) {
                @Nonnull final FluidState fluidState = FluidState.of(state);
                return fluidState.isSource() && sourceFall(world, pos, state, fluidState, world.getChunk(pos));
            }

            return false;
        }

        // helper
        public static boolean sourceFall(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final FluidState fluidState, @Nonnull final Chunk chunk) {
            @Nonnull final IBlockState here = chunk.getBlockState(pos);

            if(FluidloggedUtils.canFluidFlow(world, pos, here, fluidState.getDownDensityFace())) {
                @Nonnull final BlockPos offset = pos.offset(fluidState.getDownDensityFace());

                @Nonnull final IBlockState stateBelow = chunk.getBlockState(offset);
                @Nonnull final FluidState below = FluidloggedUtils.getFluidState(chunk, offset, stateBelow);

                if(FluidloggedUtils.isCompatibleFluid(fluidState, below) && !below.isSource()) {
                    // fluidloggable
                    if(FluidloggedUtils.isStateFluidloggable(stateBelow, world, offset, fluidState)) {
                        if(FluidloggedUtils.canFluidFlow(world, offset, stateBelow, fluidState.getUpDensityFace())) {
                            FluidloggedUtils.setFluidState(world, offset, stateBelow, fluidState, false);
                            FluidloggedUtils.setFluidToAir(world, pos, here, Constants.BlockFlags.DEFAULT);
                            return true;
                        }
                    }
                    // non-fluidloggable
                    else if(stateBelow.getBlock().isReplaceable(world, offset)) {
                        world.setBlockState(offset, state);
                        FluidloggedUtils.setFluidToAir(world, pos, here, Constants.BlockFlags.DEFAULT);
                        return true;
                    }
                }

                // special case for pyrotheum
                else if(state.getMaterial() == Material.LAVA && Blocks.FIRE.canCatchFire(world, offset)) {
                    world.setBlockState(offset, state);
                    FluidloggedUtils.setFluidToAir(world, pos, here, Constants.BlockFlags.DEFAULT);
                    return true;
                }
            }

            return false;
        }

        public static int falsify(final int i) { return 1; }
    }
}

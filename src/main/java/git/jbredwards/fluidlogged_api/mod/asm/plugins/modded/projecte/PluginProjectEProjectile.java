/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.projecte;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.util.SoundEvent;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allow ProjectE's amulet projectiles to fluidlog blocks, and fix lava placement breaking blocks
 * @author jbred
 *
 */
public final class PluginProjectEProjectile implements IASMPlugin
{
    private final boolean isWater;
    public PluginProjectEProjectile(final boolean isWaterIn) { isWater = isWaterIn; }

    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_70071_h_" : "onUpdate")) return 1;
        else return method.name.equals("apply") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final ClassNode classNode, @Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        // onUpdate
        if(index == 1) {
            // EntityWaterProjectile
            if(isWater) {
                /*
                 * Old code:
                 * PlayerHelper.checkedReplaceBlock(player, pos, Blocks.(LAVA | OBSIDIAN).getDefaultState());
                 *
                 * New code:
                 * // play vaporize effects at the changed block, instead of at the entity
                 * Hooks.mixFluid(player, pos, Blocks.(LAVA | OBSIDIAN).getDefaultState(), this.getEntityWorld());
                 */
                if(checkMethod(insn, "checkReplaceBlock")) {
                    instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                    instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, classNode.name, obfuscated ? "func_130014_f_" : "getEntityWorld", "()Lnet/minecraft/world/World;", false));
                    instructions.insertBefore(insn, genMethodNode("mixFluid", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;)Z"));
                    instructions.remove(insn);
                }
                /*
                 * Old code:
                 * this.playSound(SoundEvents.ENTITY_GENERIC_BURN, 0.5F, 2.6F + (this.getEntityWorld().rand.nextFloat() - this.getEntityWorld().rand.nextFloat()) * 0.8F);
                 *
                 * New code:
                 * // sound handled via mix effects, do nothing instead
                 * Hooks.doNothing(this, SoundEvents.ENTITY_GENERIC_BURN, 0.5F, 2.6F + (this.getEntityWorld().rand.nextFloat() - this.getEntityWorld().rand.nextFloat()) * 0.8F);
                 */
                else if(checkMethod(insn, obfuscated ? "func_184185_a" : "playSound")) {
                    instructions.insert(insn, genMethodNode("doNothing", "(Lnet/minecraft/entity/Entity;Lnet/minecraft/util/SoundEvent;FF)V"));
                    instructions.remove(insn);
                    return true;
                }
            }
            // EntityLavaProjectile
            else {
                /*
                 * Old code:
                 * block = this.getEntityWorld().getBlockState(pos).getBlock();
                 *
                 * New code:
                 * // account for FluidStates
                 * block = FluidloggedUtils.getFluidOrReal(this.getEntityWorld(), pos).getBlock();
                 */
                if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                    instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                    instructions.remove(insn);
                }
                /*
                 * Old code:
                 * if (PlayerHelper.hasBreakPermission(player, pos))
                 * {
                 *     ...
                 * }
                 *
                 * New code:
                 * // make draining account for FluidStates
                 * if (Hooks.vaporizeWater(player, pos, this.getEntityWorld()))
                 * {
                 *     ...
                 * }
                 */
                else if(checkMethod(insn, "hasBreakPermission")) {
                    instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                    instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, classNode.name, obfuscated ? "func_130014_f_" : "getEntityWorld", "()Lnet/minecraft/world/World;", false));
                    instructions.insertBefore(insn, genMethodNode("vaporizeWater", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/World;)Z"));
                    instructions.remove(insn);
                    return true;
                }
            }
        }

        // apply
        else if(index == 2) {
            /*
             * Old code:
             * if (this.world.isAirBlock(pos))
             * {
             *     ...
             * }
             *
             * New code:
             * // move check into block placement call
             * if (Hooks.alwaysTrue(this.world, pos))
             * {
             *     ...
             * }
             */
            if(isWater && checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock")) {
                instructions.insert(insn, genMethodNode("alwaysTrue", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
                instructions.remove(insn);
            }
            /*
             * Old code:
             * PlayerHelper.checkedPlaceBlock((EntityPlayerMP)this.getThrower(), pos, Blocks.FLOWING_WATER.getDefaultState());
             *
             * New code:
             * // allow Fluidlogging, and fix lava placement breaking blocks
             * Hooks.checkPlacedFluid((EntityPlayerMP)this.getThrower(), pos, Blocks.FLOWING_WATER.getDefaultState(), this.getEntityWorld());
             */
            else if(checkMethod(insn, "checkedPlaceBlock")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, classNode.name, obfuscated ? "func_130014_f_" : "getEntityWorld", "()Lnet/minecraft/world/World;", false));
                instructions.insertBefore(insn, genMethodNode("checkPlacedFluid", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;)Z"));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean alwaysTrue(@Nonnull final IBlockAccess world, @Nonnull final BlockPos pos) { return true; }
        public static boolean checkPlacedFluid(@Nonnull final EntityPlayer player, @Nonnull final BlockPos pos, @Nonnull final IBlockState fluid, @Nonnull final World world) {
            if(world.getMinecraftServer() != null && world.getMinecraftServer().isBlockProtected(world, pos, player)) return false;

            @Nonnull final IBlockState state = world.getBlockState(pos);
            if(state.getBlock().isAir(state, world, pos)) {
                if(world.provider.doesWaterVaporize()) {
                    @Nonnull final FluidStack fluidStack = FluidState.of(fluid).createFluidStack();
                    if(fluidStack.getFluid().doesVaporize(fluidStack)) {
                        FluidloggedUtils.playVaporizeEffects(world, pos, fluidStack);
                        return true;
                    }
                }

                return world.setBlockState(pos, fluid);
            }

            @Nonnull final FluidState fluidState = FluidState.of(fluid);
            return FluidloggedUtils.isStateFluidloggable(state, world, pos, fluidState) && FluidloggedUtils.setFluidState(world, pos, state, fluidState, true);
        }

        public static void doNothing(@Nonnull final Entity entity, @Nonnull final SoundEvent soundIn, final float volume, final float pitch) {}
        public static boolean mixFluid(@Nonnull final EntityPlayer player, @Nonnull final BlockPos pos, @Nonnull final IBlockState toPlace, @Nonnull final World world) {
            if(world.getMinecraftServer() != null && !world.getMinecraftServer().isBlockProtected(world, pos, player) && world.setBlockState(pos, toPlace)) {
                Blocks.LAVA.triggerMixEffects(world, pos);
                return true;
            }

            return false;
        }

        public static boolean vaporizeWater(@Nonnull final EntityPlayer player, @Nonnull final BlockPos pos, @Nonnull final World world) {
            if(world.getMinecraftServer() != null && !world.getMinecraftServer().isBlockProtected(world, pos, player)
            && FluidloggedUtils.setFluidToAir(world, pos, null, Constants.BlockFlags.DEFAULT)) {
                FluidRegistry.WATER.vaporize(null, world, pos, new FluidStack(FluidRegistry.WATER, Fluid.BUCKET_VOLUME)); // don't play client-side effects
            }

            return false;
        }
    }
}

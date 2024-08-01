/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cofhcore;

import cofh.core.fluid.BlockFluidInteractive;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.IFluidBlock;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Set;

/**
 * check FluidloggedUtils::canFluidFlow before running fluid block interactions
 * @author jbred
 *
 */
public final class PluginCoFHCore implements IASMPlugin
{
    final boolean isInteractive;
    public PluginCoFHCore(final boolean isInteractiveIn) { isInteractive = isInteractiveIn; }

    @Override
    public int getMethodIndex(@Nonnull final MethodNode method, final boolean obfuscated) {
        if(method.name.equals("handleFogDensityEvent")) return 1;
        else return method.name.equals("handleFluidBlockOverlayEvent") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            /*
             * handleFogDensityEvent:
             * Old code:
             * if (player.world.getBlockState(pos).getBlock() instanceof cofh.core.fluid.BlockFluidCore)
             * {
             *     ...
             * }
             *
             * New code:
             * // Use event IBlockState (which is gathered based on fluid collision and is FluidState-sensitive)
             * if (event.getState().getBlock() instanceof cofh.core.fluid.BlockFluidCore)
             * {
             *     ...
             * }
             */
            if(index == 1) {
                instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/client/event/EntityViewRenderEvent", "getState", "()Lnet/minecraft/block/state/IBlockState;", false));
                instructions.insert(insn, new VarInsnNode(ALOAD, 1));
                removeFrom(instructions, insn, -3);
                return true;
            }
            /*
             * handleFluidBlockOverlayEvent:
             * Old code:
             * IBlockState state = player.world.getBlockState(pos)
             *
             * New code:
             * // Account for FluidStates
             * IBlockState state = FluidloggedUtils.getFluidOrReal(player.world, pos)
             */
            else if(index == 2) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if(classNode.name.endsWith("EventHandlerRender")) return true;
        else if(isInteractive) {
            /*
             * Accessor for BlockFluidInteractive::interactWithBlock
             */
            classNode.interfaces.add(getAccessorClass());
            addMethod(classNode, "interactWithBlock_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V", null, null, generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitMethodInsn(INVOKEVIRTUAL, "cofh/core/fluid/BlockFluidInteractive", "interactWithBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V", false);
            });
            /*
             * checkForInteraction:
             * New code:
             * // check FluidloggedUtils::canFluidFlow before running fluid block interactions
             * @ASMOverwrite
             * protected void checkForInteraction(World world, BlockPos pos)
             * {
             *     Hooks.checkForInteraction(this, world, pos);
             * }
             */
            overrideMethod(classNode, method -> method.name.equals("checkForInteraction"), "checkForInteraction", withAccessorClass("(L%s;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V"), generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            });
        }
        else {
            /*
             * isEntityInsideMaterial:
             * New code:
             * // use this mod's fluid collision improvements
             * @ASMOverwrite
             * public Boolean isEntityInsideMaterial(IBlockAccess world, BlockPos blockpos, IBlockState iblockstate, Entity entity, double yToTest, Material materialIn, boolean testingHead)
             * {
             *     return Hooks.isInsideMaterial(this, world, blockPos, iblockstate, entity, yToTest, materialIn, testingHead);
             * }
             */
            overrideMethod(classNode, method -> method.name.equals("isEntityInsideMaterial"), "isInsideMaterial", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(DLOAD, 5);
                generator.visitVarInsn(ALOAD, 7);
                generator.visitVarInsn(ILOAD, 8);
            });
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static final ThreadLocal<BlockPos> fluid_pos_access = new ThreadLocal<>();
        public static void checkForInteraction(@Nonnull final Accessor fluid, @Nonnull final World world, @Nonnull final BlockPos pos) {
            @Nonnull final FluidCache cache = new FluidCache(world, pos, 2, 2);
            if(FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(cache, pos).getFluid(), fluid.getFluid())) {
                // fix bad fluid mixing checks
                if(FluidloggedAPIConfig.fixBadFluidMixing) {
                    @Nonnull final Set<BlockPos> interactedCorners = new HashSet<>();
                    for(@Nonnull final EnumFacing side : EnumFacing.HORIZONTALS) {
                        @Nonnull final BlockPos offset = pos.offset(side);
                        if(hasInteraction((BlockFluidInteractive)fluid, cache, pos, offset, side)) {
                            interactWithBlock(fluid, world, offset, pos);

                            // ---------------------------------------------
                            // CoFHCore fluid blocks can interact diagonally
                            // ---------------------------------------------

                            @Nonnull final EnumFacing cornerCW = side.rotateY();
                            @Nonnull final BlockPos offsetCW = offset.offset(cornerCW);
                            if(hasInteraction((BlockFluidInteractive)fluid, cache, offset, offsetCW, cornerCW) && !interactedCorners.contains(offsetCW)) {
                                interactWithBlock(fluid, world, offsetCW, pos);
                                interactedCorners.add(offsetCW);
                            }

                            @Nonnull final EnumFacing cornerCCW = side.rotateYCCW();
                            @Nonnull final BlockPos offsetCCW = offset.offset(cornerCCW);
                            if(hasInteraction((BlockFluidInteractive)fluid, cache, offset, offsetCCW, cornerCCW) && !interactedCorners.contains(offsetCCW)) {
                                interactWithBlock(fluid, world, offsetCCW, pos);
                                interactedCorners.add(offsetCCW);
                            }
                        }
                    }
                }

                // check all 8 blocks horizontally surrounding this one, and ignore fluid flow logic
                else {
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX()    , pos.getY(), pos.getZ() - 1), pos);
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX()    , pos.getY(), pos.getZ() + 1), pos);
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX() - 1, pos.getY(), pos.getZ()    ), pos);
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX() + 1, pos.getY(), pos.getZ()    ), pos);
                    // CoFHCore fluid blocks can interact diagonally
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX() - 1, pos.getY(), pos.getZ() - 1), pos);
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX() + 1, pos.getY(), pos.getZ() - 1), pos);
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX() - 1, pos.getY(), pos.getZ() + 1), pos);
                    interactWithBlock(fluid, world, cache.mutablePos.setPos(pos.getX() + 1, pos.getY(), pos.getZ() + 1), pos);
                }
            }
        }

        // helper
        public static boolean hasInteraction(@Nonnull final BlockFluidInteractive fluid, @Nonnull final FluidCache cache, @Nonnull final BlockPos origin, @Nonnull final BlockPos offset, @Nonnull final EnumFacing side) {
            if(!FluidloggedUtils.canFluidFlow(cache, origin, cache.getBlockState(origin), side)) return false;
            else if(!cache.isAirBlock(offset) && fluid.hasInteraction(FluidloggedUtils.getFluidState(cache, offset).getState())) return
                    cache.getBlockState(offset).getBlock().isReplaceable(cache, offset) &&
                    FluidloggedUtils.canFluidFlow(cache, offset, cache.getBlockState(offset), side.getOpposite());

            else return true;
        }

        // helper
        public static void interactWithBlock(@Nonnull final Accessor fluid, @Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final BlockPos fluidPos) {
            fluid_pos_access.set(fluidPos);
            fluid.interactWithBlock_Public(world, pos);
            fluid_pos_access.set(null);
        }

        @Nullable
        public static Boolean isInsideMaterial(@Nonnull final BlockFluidBase fluid, @Nonnull final IBlockAccess worldIn, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Entity entity, final double yToTest, @Nonnull final Material materialIn, final boolean testingHead) {
            return fluid.getDensity() < 0 ? Boolean.FALSE : FluidCollisionHandler.isEntityInsideMaterial(worldIn, pos, state, entity, yToTest, testingHead ? state.getMaterial() : materialIn, testingHead);
        }
    }

    public interface Accessor extends IFluidBlock
    {
        void interactWithBlock_Public(@Nonnull final World world, @Nonnull final BlockPos pos);
    }
}

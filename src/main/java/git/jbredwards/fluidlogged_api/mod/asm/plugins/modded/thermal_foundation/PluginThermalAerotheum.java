/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.thermal_foundation;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidFlowHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.impl.SpecializedFluidNeighborInfo;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * fix conflicts
 * @author jbred
 *
 */
public final class PluginThermalAerotheum implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IConditionalFluid");
        addMethod(classNode, "cannotFlowAt", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)Z",
            "cannotFlowAt", "(Lnet/minecraft/util/math/BlockPos;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 2);
                generator.visitFieldInsn(GETSTATIC, classNode.name, "maxHeight", "I");
            }
        );
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "update", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;ZZI)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitFieldInsn(GETSTATIC, classNode.name, "enableSourceDissipate", "Z");
                generator.visitFieldInsn(GETSTATIC, classNode.name, "enableSourceFloat", "Z");
                generator.visitFieldInsn(GETSTATIC, classNode.name, "maxHeight", "I");
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean cannotFlowAt(@Nonnull final BlockPos pos, final int maxHeight) {
            return pos.getY() > maxHeight;
        }

        public static void update(@Nonnull final World world, @Nonnull final BlockPos pos, @Nonnull final IBlockState state, @Nonnull final Random rand, final boolean dissipate, final boolean doFloat, final int maxHeight) {
            @Nonnull final FluidState fluidState = FluidState.of(state);
            if(fluidState.isSource()) {
                // source block dissipate (thermal foundation functionality)
                final int densityDir = fluidState.getDensityDir();
                if(dissipate && (pos.getY() + densityDir > maxHeight || pos.getY() + densityDir > maxHeight * 0.8 && new SpecializedFluidNeighborInfo.Forge(world, pos, fluidState, 0).canFlowInto(0, 0, 0, fluidState.getMetadata(), fluidState.getDownDensityFace(), true, false))) {
                    FluidloggedUtils.setFluidToAir(world, pos, null, Constants.BlockFlags.DEFAULT);
                    return;
                }

                // source block floating (thermal foundation functionality)
                if(doFloat && rand.nextInt(3) == 0 && PluginThermalFoundation.Hooks.sourceFall(world, pos, state, fluidState, world.getChunk(pos))) return;
            }

            else if(cannotFlowAt(pos, maxHeight)) {
                FluidloggedUtils.setFluidToAir(world, pos, null, Constants.BlockFlags.DEFAULT);
                return;
            }

            FluidFlowHandler.updateClassic(world, pos, fluidState);
        }
    }
}

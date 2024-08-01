/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.cb_multipart;

import codechicken.multipart.BlockMultipart$;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import codechicken.multipart.minecraft.McMetaPart;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiPredicate;

/**
 * allows multipart blocks to be placed in fluids
 * @author jbred
 *
 */
public final class PluginCBMultipart implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("place"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * place:
         * Old code: (scala)
         * val hit = RayTracer.retrace(player)
         *
         * New code: (scala)
         * //don't stop the raytrace when it comes in contact with a fluid
         * val hit = RayTracer.retrace(player, false)
         */
        if(checkMethod(insn, "retrace")) {
            //change method to one with a flexible `stopAtFluid` flag & set it to false
            ((MethodInsnNode)insn).desc = "(Lnet/minecraft/entity/player/EntityPlayer;Z)Lnet/minecraft/util/math/RayTraceResult;";
            instructions.insertBefore(insn, new InsnNode(ICONST_0));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        if("codechicken/multipart/BlockMultipart".equals(classNode.name)) {
            classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
            addMethod(classNode, "isFluidloggable", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)Z",
                "isFluidloggable", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)Z", generator -> {
                    generator.visitVarInsn(ALOAD, 1);
                    generator.visitVarInsn(ALOAD, 2);
                    generator.visitVarInsn(ALOAD, 3);
                    generator.visitVarInsn(ALOAD, 4);
                }
            );

            classNode.methods.removeIf(method -> method.name.equals("isAir")); // allow multipart blocks to be fluidloggable between block set & tile set
            return false;
        }

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static final Map<ResourceLocation, BiPredicate<TMultiPart, FluidState>> PART_CONFIG = new HashMap<>();
        public static boolean isFluidloggable(@Nonnull final IBlockState state, @Nonnull final IBlockAccess world, @Nonnull final BlockPos pos, @Nonnull final FluidState fluidState) {
            @Nullable final TileMultipart multipart = BlockMultipart$.MODULE$.getTile(world, pos);
            if(multipart != null && !multipart.partList().isEmpty()) {
                for(@Nonnull final TMultiPart part : multipart.jPartList()) {
                    @Nullable final BiPredicate<TMultiPart, FluidState> predicate = PART_CONFIG.get(part.getType());
                    if(predicate != null) return predicate.test(part, fluidState); // let users add overrides to specialized multiparts (like translocators)
                    else if(!(part instanceof McMetaPart) ? !FluidloggedAPIConfig.allowDefaults // by default, assume any non-state-based multipart is fluidloggable
                    :!FluidloggedUtils.isStateFluidloggable(((McMetaPart)part).state, world, pos, fluidState)) return false;
                }
            }

            return true;
        }
    }
}

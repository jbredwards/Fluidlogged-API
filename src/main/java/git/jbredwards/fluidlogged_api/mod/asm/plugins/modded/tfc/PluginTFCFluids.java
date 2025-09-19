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

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.tfc;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.fluid.ICompatibleFluid;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * use ICompatibleFluid for water-like fluids
 * @author jbred
 *
 */
public final class PluginTFCFluids implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("registerFluids"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * registerFluids:
         * Old code:
         * registerFluid(new Fluid(...))
         *
         * New code:
         * //use ICompatibleFluid for water-like fluids
         * registerFluid(new TFCWaterFluid(...))
         */
        if(insn.getOpcode() == NEW) {
            ((TypeInsnNode)insn).desc = "git/jbredwards/fluidlogged_api/mod/asm/plugins/modded/tfc/PluginTFCFluids$TFCWaterFluid";
            ((MethodInsnNode)getNext(insn, 6)).owner = "git/jbredwards/fluidlogged_api/mod/asm/plugins/modded/tfc/PluginTFCFluids$TFCWaterFluid";
            return ((LdcInsnNode)getNext(insn, 2)).cst.equals("salt_water");
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class TFCWaterFluid extends Fluid implements ICompatibleFluid
    {
        public TFCWaterFluid(@Nonnull String fluidName, @Nonnull ResourceLocation still, @Nonnull ResourceLocation flowing, int color) {
            super(fluidName, still, flowing, color);
        }

        @Nonnull
        @Override
        public Fluid getParentFluid() { return FluidRegistry.WATER; }
    }
}

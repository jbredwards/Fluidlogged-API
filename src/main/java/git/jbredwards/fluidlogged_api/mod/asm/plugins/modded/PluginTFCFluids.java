package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

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
            ((TypeInsnNode)insn).desc = "git/jbredwards/fluidlogged_api/mod/asm/plugins/modded/PluginTFCFluids$TFCWaterFluid";
            ((MethodInsnNode)getNext(insn, 6)).owner = "git/jbredwards/fluidlogged_api/mod/asm/plugins/modded/PluginTFCFluids$TFCWaterFluid";
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

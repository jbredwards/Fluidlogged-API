package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * MCWorldBlock.getFluid can read FluidStates
 * @author jbred
 *
 */
public final class PluginCraftTweaker implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("getFluid"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getFluid:
         * Old code:
         * return CraftTweakerMC.getILiquidDefinition(FluidRegistry.lookupFluidForBlock(blocks.getBlockState(pos).getBlock()));
         *
         * New code:
         * allow CraftTweaker scripts to access FluidStates
         * return CraftTweakerMC.getILiquidDefinition(FluidloggedUtils.getFluidState(this.blocks, this.pos).getFluid());
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", false));
            removeFrom(instructions, insn, 2);
            return true;
        }

        return false;
    }
}

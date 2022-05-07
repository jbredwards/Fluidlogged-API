package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix BOP fluid block mixing
 * @author jbred
 *
 */
public final class PluginBiomesOPlenty implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("checkForMixing"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //check if can flow
        if(checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
            instructions.insert(insn, genMethodNode("canBOPFluidFlow", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z"));
            removeFrom(instructions, insn, -3);
        }
        //account for FluidState
        else if(checkMethod(insn.getNext(), obfuscated ? "func_177230_c" : "getBlock")) {
            instructions.insert(insn, genMethodNode("getBOPFluidOrAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, new MethodInsnNode(INVOKESPECIAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

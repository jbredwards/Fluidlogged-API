package git.jbredwards.fluidlogged_api.asm.plugin.forge;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public class BlockLiquidWrapperPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return "fill";
    }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraftforge/fluids/FluidStack;Z)I";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //setBlockState, line 83
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, method("placeVanilla", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;ILnet/minecraftforge/fluids/FluidStack;)V"));
            instructions.remove(insn.getNext());
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

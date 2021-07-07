package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * fixes optifine shaders with fluidlogged fluid blocks
 * @author jbred
 *
 */
public class BlockModelRendererPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, "pushEntity", null)) {
            instructions.insert(ASMUtils.getPrevious(insn, 4), method("correctFluidloggedFluidShader", "(Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
        }
        if(ASMUtils.checkMethod(insn, "getRenderEnv", null)) {
            instructions.insert(ASMUtils.getPrevious(insn, 2), method("correctFluidloggedFluidShader", "(Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;"));
        }

        return false;
    }

    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        return true;
    }
}

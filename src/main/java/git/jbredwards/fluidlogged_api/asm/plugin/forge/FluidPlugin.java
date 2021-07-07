package git.jbredwards.fluidlogged_api.asm.plugin.forge;

import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class FluidPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        return method.name.equals("setBlock") || method.name.equals("setOriginalBlock"); //setOriginalBlock for betweenlands fix
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //registers a fluid-logged block for each fluid with a block
        if(insn.getOpcode() == PUTFIELD) {
            //adds code below line 188 in the Fluid class
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //fluid variable
            list.add(new VarInsnNode(ALOAD, 0));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "registerFluidloggedBlock", "(Lnet/minecraftforge/fluids/Fluid;)V", false));

            instructions.insert(insn, list);
        }
        //fixes console spam
        if(insn.getOpcode() == INVOKEINTERFACE) {
            //adds code at line 192 in the Fluid class
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/asm/ASMHooks", "fluidBlockErrorSpamFix", "(Lorg/apache/logging/log4j/Logger;Ljava/lang/String;Lnet/minecraft/block/Block;Ljava/lang/String;Lnet/minecraft/block/Block;)V", false));

            instructions.insert(insn, list);
            instructions.remove(insn);

            return true;
        }

        return false;
    }
}

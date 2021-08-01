package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * removes timestop's object holders, the mod should be using ASM instead for mod compat
 * @author jbred
 *
 */
public class TimestopPlugin extends AbstractMultiMethodPlugin
{
    public boolean removeAnnotations = false;

    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //RegistryHandler::onBlockRegister
        if(ASMUtils.checkMethod(method, "onBlockRegister", null)) {
            currentMethod = 1;
            return true;
        }
        //bucketUsed::onBucketUse
        if(ASMUtils.checkMethod(method, "onBucketUse", null)) {
            currentMethod = 2;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //removes the object holders from the RegistryHandler class
        if(currentMethod == 1 && insn.getOpcode() == RETURN) {
            while(insn.getPrevious() != null) instructions.remove(insn.getPrevious());
            removeAnnotations = true;
            return true;
        }
        //removes the bucketUsed event from the Main class
        if(currentMethod == 2 && ASMUtils.checkMethod(insn, "getTarget", "()Lnet/minecraft/util/math/RayTraceResult;")) {
            instructions.insert(insn, method("TSCompat", "(Lnet/minecraftforge/event/entity/player/FillBucketEvent;)Lnet/minecraft/util/math/RayTraceResult;"));
            instructions.remove(insn);
            removeAnnotations = false;
            return true;
        }

        removeAnnotations = false;
        return false;
    }

    @Override
    public byte[] transform(byte[] basicClass, boolean obfuscated) {
        basicClass = super.transform(basicClass, obfuscated);
        //removes the annotations
        if(removeAnnotations) return ASMUtils.removeAnnotations(basicClass);
        //doesn't remove the annotations
        else return basicClass;
    }
}

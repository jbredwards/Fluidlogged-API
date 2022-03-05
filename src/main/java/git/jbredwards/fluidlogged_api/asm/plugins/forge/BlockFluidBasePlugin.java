package git.jbredwards.fluidlogged_api.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class BlockFluidBasePlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //constructor
        if(checkMethod(method, "<init>", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V"))
            return 1;
        //defaultDisplacements clinit
        else if(checkMethod(method, "<clinit>", "()V"))
            return 2;

        //default
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //constructor, line 181
        if(index == 1 && checkField(insn, "defaultDisplacements", "Ljava/util/Map;")) {
            instructions.insert(insn, genMethodNode("defaultDisplacements", "(Ljava/util/Map;)Ljava/util/Map;"));
            return true;
        }
        //clinit, line 73-113
        else if(index == 2 && insn.getNext().getOpcode() == LDC) {
            //removes all default entries, as the class is now loaded before they're registered
            while(insn.getPrevious().getOpcode() != PUTSTATIC) instructions.remove(insn.getPrevious());
            return true;
        }

        return false;
    }
}

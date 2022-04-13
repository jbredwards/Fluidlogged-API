package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * makes liquids fluidloggable
 * @author jbred
 *
 */
public final class PluginBlockLiquid implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_149645_b" : "getRenderType")) return 1;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //change the render type to a model
        if(index == 1 && checkField(insn, "LIQUID")) ((FieldInsnNode)insn).name = "MODEL";

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //classNode.interfaces.add("get/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid");

        return true;
    }
}

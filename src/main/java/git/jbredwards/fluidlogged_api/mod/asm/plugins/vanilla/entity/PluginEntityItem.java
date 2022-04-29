package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * handle lava collisions correctly
 * @author jbred
 *
 */
public final class PluginEntityItem implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_70071_h_" : "onUpdate"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //use world#isMaterialInBB rather than world#getBlockState
        if(checkField(insn, obfuscated ? "field_151587_i" : "LAVA")) {
            ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
            removeFrom(instructions, insn.getPrevious(), -2);
            removeFrom(instructions, getPrevious(insn, 2), -1);
            instructions.insertBefore(insn, new MethodInsnNode(INVOKESPECIAL, "net/minecraft/entity/Entity", obfuscated ? "func_174813_aQ" : "getEntityBoundingBox", "()Lnet/minecraft/util/math/AxisAlignedBB;", false));
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_72875_a" : "isMaterialInBB", "(Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Z", false));
            return true;
        }

        return false;
    }
}

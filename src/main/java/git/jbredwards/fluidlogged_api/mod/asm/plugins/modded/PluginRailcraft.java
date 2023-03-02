package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix railcraft uncraftable potion bug when collecting water bottles (issue#148)
 * @author jbred
 *
 */
public final class PluginRailcraft implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("<init>"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * Constructor:
         * Old code:
         * containerTable.put(Items.GLASS_BOTTLE.delegate, "water", Items.POTIONITEM.delegate);
         *
         * New code:
         * //remove water bottles from the containerTable
         * ...
         */
        if(checkMethod(insn.getPrevious(), "put")) {
            removeFrom(instructions, insn, -8);
            return true;
        }

        return false;
    }
}

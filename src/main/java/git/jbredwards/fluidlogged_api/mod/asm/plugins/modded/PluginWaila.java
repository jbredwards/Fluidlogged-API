package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * remove duplicate handlers for BlockLiquid
 * @author jbred
 *
 */
public final class PluginWaila implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("register"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * register:
         * Old code:
         * registrar.registerStackProvider(HUDHandlerFluids.INSTANCE, BlockLiquid.class);
         * registrar.registerHeadProvider(HUDHandlerFluids.INSTANCE, BlockLiquid.class);
         * registrar.registerTailProvider(HUDHandlerFluids.INSTANCE, BlockLiquid.class);
         *
         * New code:
         * //remove duplicate handlers for BlockLiquid
         * ...
         */
        if(insn.getOpcode() == LDC && ((LdcInsnNode)insn).cst.equals(Type.getType("Lnet/minecraft/block/BlockLiquid;"))) {
            final boolean isDone = checkMethod(insn.getNext(), "registerTailProvider");
            instructions.remove(insn.getNext());
            removeFrom(instructions, insn, -3);
            return isDone;
        }

        return false;
    }
}

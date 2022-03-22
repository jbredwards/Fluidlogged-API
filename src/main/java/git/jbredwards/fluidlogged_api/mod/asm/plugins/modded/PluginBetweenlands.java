package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import java.util.Iterator;

/**
 * the betweenlands mod overrides most forge fluid methods to implement its own sudo fluidlogging,
 * this mod fixes everything betweenlands did already, so this undoes all of its stuff
 * @author jbred
 *
 */
public final class PluginBetweenlands implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "canDisplace", null)
                || checkMethod(method, "displaceIfPossible", null)
                || checkMethod(method, "getFluidHeightForRender", null)
                || checkMethod(method, "getFlowVector", null)
                || checkMethod(method, "causesDownwardCurrent", null)
                || checkMethod(method, "getQuantaValue", null)
                || checkMethod(method, "canFlowInto", null)
                || checkMethod(method, obfuscated ? "func_180650_b" : "updateTick", null)
                || checkMethod(method, obfuscated ? "func_176225_a" : "shouldSideBeRendered", null))
            return 1;

        else return checkMethod(method, obfuscated ? "func_176200_f" : "isReplaceable", null) ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            //add this.getDefaultState()
            instructions.insert(insn, new MethodInsnNode(INVOKESPECIAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));

            //remove world.getBlockState(pos)
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean removeMethod(@Nonnull Iterator<MethodNode> methods, boolean obfuscated, int index) {
        if(index == 1) {
            methods.remove();
            return true;
        }

        return false;
    }
}

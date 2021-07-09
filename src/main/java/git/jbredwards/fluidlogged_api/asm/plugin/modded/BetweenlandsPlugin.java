package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

import java.util.Iterator;

/**
 * the betweenlands mod by default has the need to override most forge fluid methods,
 * this mod fixes everything betweenlands did already, so this undoes all of its stuff
 * @author jbred
 *
 */
public class BetweenlandsPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        return     method.name.equals("canDisplace")
                || method.name.equals("displaceIfPossible")
                || method.name.equals("getFluidHeightForRender")
                || method.name.equals("getFlowVector")
                || method.name.equals("causesDownwardCurrent")
                || method.name.equals("getQuantaValue")
                || method.name.equals("canFlowInto")
                || method.name.equals("updateTick")
                || method.name.equals("func_180650_b");
    }

    @Override
    public boolean removeMethod(Iterator<MethodNode> it, boolean obfuscated) {
        it.remove();
        return true;
    }

    //unused
    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) { return true; }
}

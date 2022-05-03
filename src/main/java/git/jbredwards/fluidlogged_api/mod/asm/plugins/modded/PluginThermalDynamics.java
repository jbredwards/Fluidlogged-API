package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix block raytrace to ignore fluids
 * @author jbred
 *
 */
public final class PluginThermalDynamics implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("openGui"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, "retrace")) {
            //change method to one with a flexible `stopAtFluid` flag & set it to false
            ((MethodInsnNode)insn).desc = "(Lnet/minecraft/entity/player/EntityPlayer;Z)Lnet/minecraft/util/math/RayTraceResult;";
            instructions.insertBefore(insn, new InsnNode(ICONST_0));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //remove an unnecessary method which only exists to cause conflicts /s
        classNode.methods.removeIf(method -> method.name.equals("getExplosionResistance"));
        return true;
    }
}

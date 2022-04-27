package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix all fluid-related rain collisions
 * @author jbred
 *
 */
public final class PluginParticleRain implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_189213_a" : "onUpdate"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //disable built-in liquid check
        if(insn.getOpcode() == INSTANCEOF) ((TypeInsnNode)insn).desc = "java/lang/String";

        //add new check
        else if(checkMethod(insn, obfuscated ? "func_185900_c" : "getBoundingBox")) {
            instructions.insert(insn, genMethodNode("fixRainCollision", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/util/math/AxisAlignedBB;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

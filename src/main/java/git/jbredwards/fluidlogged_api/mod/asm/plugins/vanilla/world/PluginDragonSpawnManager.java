package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * summoning the ender dragon will now void FluidStates at the pillar locations
 * @author jbred
 *
 */
public final class PluginDragonSpawnManager implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_186079_a" : "process"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            final InsnList list = new InsnList();
            list.add(new FieldInsnNode(GETSTATIC, "net/minecraft/init/Blocks", obfuscated ? "field_150350_a" : "AIR", "Lnet/minecraft/block/Block;"));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175656_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z", false));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

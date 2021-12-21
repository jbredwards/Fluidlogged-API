package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class DragonSpawnManagerPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, obfuscated ? "func_186079_a" : "process", null) ? 1 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(index == 1 && checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir", null)) {
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

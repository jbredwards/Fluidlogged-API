package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes some lighting & explosion related issues
 * @author jbred
 *
 */
public final class BlockPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //getLightValue, line 1250
        if(checkMethod(method, "getLightValue", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I")) {
            setMaxLocals(method, 3);
            return 1;
        }
        //getExplosionResistance, line 1766
        if(checkMethod(method, "getExplosionResistance", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/Explosion;)F")) {
            setMaxLocals(method, 5);
            return 2;
        }
        //getLightOpacity, line 2030
        if(checkMethod(method, "getLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I")) {
            setMaxLocals(method, 3);
            return 3;
        }

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //getLightValue, line 1250
        if(index == 1 && checkMethod(insn, obfuscated ? "func_149750_m" : "getLightValue", "(Lnet/minecraft/block/state/IBlockState;)I")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            //adds the new code
            list.add(genMethodNode("getLightValue", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //getExplosionResistance, line 1766
        if(index == 2 && checkMethod(insn, obfuscated ? "func_149638_a" : "getExplosionResistance", "(Lnet/minecraft/entity/Entity;)F")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 4));
            //adds the new code
            list.add(genMethodNode("getExplosionResistance", "(Lnet/minecraft/block/Block;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/Explosion;)F"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //getLightOpacity, line 2030
        if(index == 3 && checkMethod(insn, obfuscated ? "func_149717_k" : "getLightOpacity", "(Lnet/minecraft/block/state/IBlockState;)I")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            //adds the new code
            list.add(genMethodNode("getLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}
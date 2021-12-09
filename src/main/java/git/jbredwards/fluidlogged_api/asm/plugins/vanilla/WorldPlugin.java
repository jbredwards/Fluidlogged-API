package git.jbredwards.fluidlogged_api.asm.plugins.vanilla;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * corrects a lot of fluid related interactions
 * @author jbred
 *
 */
public final class WorldPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //setBlockState, line 401
        if(checkMethod(method, obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
            return 1;

        //setBlockToAir, line 456
        else if(checkMethod(method, obfuscated ? "func_175698_g" : "setBlockToAir", null))
            return 2;

        //destroyBlock, line 480
        else if(checkMethod(method, obfuscated ? "func_175655_b" : "destroyBlock", null))
            return 2;

        //handleMaterialAcceleration, line 2443
        else if(checkMethod(method, obfuscated ? "func_72918_a"  : "handleMaterialAcceleration", null)) {
            setMaxLocals(method, 12);
            return 3;
        }

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(checkMethod(method, obfuscated ? "func_72953_d"  : "containsAnyLiquid", null)
        || checkMethod(method, obfuscated ? "func_147470_e" : "isFlammableWithin", null)
        || checkMethod(method, obfuscated ? "func_72875_a"  : "isMaterialInBB", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null)) return 4;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //setBlockState, line 401
        if(index == 1 && checkMethod(insn, obfuscated ? "func_177436_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            //oldState local var
            list.add(new VarInsnNode(ALOAD, 6));
            //params
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ILOAD, 3));

            list.add(genMethodNode("setBlockState", "(Lnet/minecraft/world/chunk/Chunk;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;I)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        //setBlockToAir & destroyBlock
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("getFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));

            instructions.insert(insn, list);
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }

        //handleMaterialAcceleration, line 2443
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_185344_t" : "release", "()V")) {
            final InsnList list = new InsnList();
            //params
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            //vec3d
            list.add(new VarInsnNode(ALOAD, 11));
            //flag
            list.add(new VarInsnNode(ILOAD, 10));
            //aabb positions
            list.add(new VarInsnNode(ILOAD, 4));
            list.add(new VarInsnNode(ILOAD, 5));
            list.add(new VarInsnNode(ILOAD, 6));
            list.add(new VarInsnNode(ILOAD, 7));
            list.add(new VarInsnNode(ILOAD, 8));
            list.add(new VarInsnNode(ILOAD, 9));
            //adds new code
            list.add(genMethodNode("handleMaterialAcceleration", "(Lnet/minecraft/util/math/BlockPos$PooledMutableBlockPos;Lnet/minecraft/world/World;Lnet/minecraft/block/material/Material;Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/Vec3d;ZIIIIII)Z"));
            //set flag to new code value
            list.add(new VarInsnNode(ISTORE, 10));

            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(index == 4 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/common/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

package git.jbredwards.fluidlogged_api.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes the following issues:
 * -vanilla fluids no longer do block mixing when they shouldn't
 * -vanilla fluids now flow from fluidlogged blocks
 * @author jbred
 *
 */
public final class BlockDynamicLiquidPlugin implements IASMPlugin
{
    @Override
    public int isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        //placeStaticBlock, line 24
        if(checkMethod(method, obfuscated ? "func_180690_f" : "placeStaticBlock", null)) {
            setMaxLocals(method, 4);
            return 1;
        }
        //updateTick
        else if(checkMethod(method, obfuscated ? "func_180650_b" : "updateTick", null))
            return 2;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //placeStaticBlock, line 24
        if(index == 1 && checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, genMethodNode("placeStaticBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;ILnet/minecraft/block/Block;)Z"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            return true;
        }
        //updateTick
        else if(index == 2) {
            //line 47
            if(checkMethod(insn, obfuscated ? "func_176371_a" : "checkAdjacentBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)I")) {
                final InsnList list = new InsnList();
                //params
                list.add(new VarInsnNode(ALOAD, 2));
                //EnumFacing
                list.add(new VarInsnNode(ALOAD, 10));
                //adds new code
                list.add(genMethodNode("checkAdjacentBlock", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ILnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)I"));
                instructions.insert(insn, list);
                instructions.remove(insn);
                return false;
            }
            //line 57
            else if(checkMethod(insn, obfuscated ? "func_189542_i" : "getDepth", "(Lnet/minecraft/block/state/IBlockState;)I")) {
                final InsnList list = new InsnList();
                //params
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 2));
                //adds new code
                list.add(genMethodNode("getDepth", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I"));
                instructions.insert(insn, list);
                instructions.remove(insn);
                return false;
            }
        }

        return false;
    }
}

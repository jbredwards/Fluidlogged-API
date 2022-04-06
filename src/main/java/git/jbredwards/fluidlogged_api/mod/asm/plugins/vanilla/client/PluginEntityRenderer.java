package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * -fixes underwater block selection, for real though, it's hard-coded to not render underwater, but if I remove that, works fine underwater, WHY?! lol
 * -lava FluidStates now emit smoke while raining
 * -fixes FluidState fog color
 * @author jbred
 *
 */
public final class PluginEntityRenderer implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //renderWorldPass, line 1409
        if(checkMethod(method, obfuscated ? "func_175068_a" : "renderWorldPass", "(IFJ)V"))
            return 1;

        //addRainParticles, line 1561
        else if(checkMethod(method, obfuscated ? "func_78484_h" : "addRainParticles", "()V"))
            return 2;

        //updateFogColor, line 1857
        else if(checkMethod(method, obfuscated ? "func_78466_h" : "updateFogColor", "(F)V"))
            return 3;

        else return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //renderWorldPass, line 1409
        //removes '!entity.isInsideOfMaterial()' at 1409 and replaces it with '!false'
        if(index == 1 && checkMethod(insn, obfuscated ? "func_70055_a" : "isInsideOfMaterial", "(Lnet/minecraft/block/material/Material;)Z")) {
            instructions.insert(insn, new InsnNode(ICONST_0));
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.remove(getPrevious(insn, 0));

            return true;
        }
        //addRainParticles, line 1561
        else if(index == 2 && checkField(insn, obfuscated ? "field_151587_i" : "LAVA", "Lnet/minecraft/block/material/Material;")) {
            final InsnList list = new InsnList();
            //adds new code
            list.add(new VarInsnNode(ALOAD, 17));
            list.add(genMethodNode("addRainParticles", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/BlockPos;)Z"));
            ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
            instructions.insert(insn, list);
            //removes old
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);

            return true;
        }
        //updateFogColor, line 1857
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

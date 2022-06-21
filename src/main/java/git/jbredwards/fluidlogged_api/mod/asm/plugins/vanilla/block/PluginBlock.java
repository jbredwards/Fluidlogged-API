package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes some lighting, canSustainPlant, and explosion related issues
 * @author jbred
 *
 */
public final class PluginBlock implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //isReplaceable
        if(method.name.equals(obfuscated ? "func_176200_f" : "isReplaceable"))
            return 1;

        //getLightValue, line 1250
        else if(checkMethod(method, "getLightValue", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I")) {
            return 2;
        }

        //removedByPlayer, line 1422
        else if(checkMethod(method, "removedByPlayer", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/player/EntityPlayer;Z)Z")) {
            return 3;
        }

        //getExplosionResistance, line 1766
        else if(checkMethod(method, "getExplosionResistance", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/Explosion;)F")) {
            return 4;
        }

        //canSustainPlant, lines 1949 & 1964
        else if(checkMethod(method, "canSustainPlant", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/common/IPlantable;)Z")){
            return 5;
        }

        //getLightOpacity, line 2030
        else if(checkMethod(method, "getLightOpacity", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I")) {
            return 6;
        }

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //isReplaceable
        if(index == 1 && checkMethod(insn, obfuscated ? "func_185904_a" : "getMaterial")) {
            //add blockMaterial#isReplaceable
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            //remove old
            instructions.remove(getPrevious(insn, 3));
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.remove(insn);
            return true;
        }
        //getLightValue, line 1250
        if(index == 2 && checkMethod(insn, obfuscated ? "func_185906_d" : "getLightValue", "()I")) {
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
        //removedByPlayer, line 1422
        else if(index == 3 && checkField(insn, obfuscated ? "field_150350_a" : "AIR", "Lnet/minecraft/block/Block;")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 3));
            //adds new code
            list.add(genMethodNode("getFluidOrAir", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insertBefore(insn, list);
            instructions.remove(insn.getNext());
            instructions.remove(insn);
            return true;
        }
        //getExplosionResistance, line 1766
        else if(index == 4 && checkMethod(insn, obfuscated ? "func_149638_a" : "getExplosionResistance", "(Lnet/minecraft/entity/Entity;)F")) {
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
        //canSustainPlant
        else if(index == 5) {
            //line 1949
            if(checkMethod(insn, obfuscated ? "func_185514_i" : "canSustainBush")) {
                final InsnList list = new InsnList();
                //params
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, 3));
                //adds the new code
                list.add(genMethodNode("canSustainPlant", "(Lnet/minecraft/block/BlockBush;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
                instructions.insert(insn, list);
                instructions.remove(insn);
                return false;
            }
            //line 1964
            else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
                return false;
            }
        }
        //getLightOpacity, line 2030
        else if(index == 6 && checkMethod(insn, obfuscated ? "func_185891_c" : "getLightOpacity", "()I")) {
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

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //used for overriding canFluidFlow behavior via the config
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "canFluidFlow", "Ljava/lang/Boolean;", null, null));
        return true;
    }
}

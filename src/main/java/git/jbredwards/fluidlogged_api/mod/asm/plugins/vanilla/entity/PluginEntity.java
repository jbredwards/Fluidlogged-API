package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class PluginEntity implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_180799_ab" : "isInLava")) return 1;
        else if(method.name.equals(obfuscated ? "func_70072_I" : "handleWaterMovement")) return 2;
        else if(method.name.equals(obfuscated ? "func_71061_d_" : "doWaterSplashEffect")) return 3;
        else if(method.name.equals(obfuscated ? "func_70055_a" : "isInsideOfMaterial")) return 4;
        else if(method.name.equals(obfuscated ? "func_145775_I" : "doBlockCollisions")) return 5;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        //don't change the AABB prior to checking lava collision
        if(index == 1 && checkMethod(insn, obfuscated ? "func_72314_b" : "grow")) {
            removeFrom(instructions, insn, -3);
            return true;
        }
        //don't change the AABB prior to checking water collision unless squid
        else if(index == 2 && checkMethod(getNext(insn, 2), obfuscated ? "func_72314_b" : "grow")) {
            instructions.insert(insn, genMethodNode("fixSquidWaterCollision", "(DLnet/minecraft/entity/Entity;)D"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            return true;
        }
        //doWaterSplashEffect
        else if(index == 3) {
            if(checkMethod(insn.getPrevious(), obfuscated ? "func_76128_c" : "floor")) {
                instructions.insert(insn, genMethodNode("doWaterSplashEffect", "(Lnet/minecraft/entity/Entity;)F"));
                removeFrom(instructions, insn, -3);
            }
            //remove offset, not needed
            else if(insn.getOpcode() == FADD && insn.getPrevious().getOpcode() == FCONST_1)
                removeFrom(instructions, insn, -1);
            //remove bubble particle motion
            else if(checkField(insn, "WATER_BUBBLE")) {
                removeFrom(instructions, getNext(insn, 15), 12);
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
            }
        }
        //isInsideOfMaterial, add FluidState functionality
        else if(index == 4 && checkMethod(insn, "isEntityInsideMaterial")) {
            instructions.insert(insn, genMethodNode("isEntityInsideFluidState", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;"));
            instructions.remove(insn);
            return true;
        }
        //doBlockCollisions, fix fluid collisions & add FluidState functionality
        else if(index == 5 && checkMethod(insn, obfuscated ? "func_180634_a" : "onEntityCollision")) {
            instructions.insert(insn, genMethodNode("onEntityCollidedWithFluidState", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;)V"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

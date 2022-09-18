package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * removes bad checks in place for built-in vanilla one
 * @author jbred
 *
 */
public final class PluginBuildersWands implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("shouldContinue"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * shouldContinue:
         * Old code:
         * if(!targetBlock.canPlaceBlockAt(world.getWorld(), new BlockPos(currentCandidate.x, currentCandidate.y, currentCandidate.z))) return false;
         * if(!targetBlock.isReplaceable(world.getWorld(), new BlockPos(currentCandidate.x, currentCandidate.y, currentCandidate.z))) return false;
         *
         * New code:
         * //remove buggy checks
         */
        if(checkMethod(insn, obfuscated ? "func_176196_c" : "canPlaceBlockAt") || checkMethod(insn, obfuscated ? "func_176200_f" : "isReplaceable")) {
            instructions.insert(insn, new InsnNode(ICONST_1));
            removeFrom(instructions, insn, -13);
            return false;
        }
        /*
         * shouldContinue:
         * Old code:
         * return !world.entitiesInBox(blockBB);
         *
         * New code:
         * //add built-in vanilla method instead
         * this.world.getWorld().mayPlace(targetBlock, new BlockPos(currentCandidate.x, currentCandidate.y, currentCandidate.z), false, facing, this.player.getPlayer());
         */
        else if(checkMethod(insn, "entitiesInBox")) {
            final InsnList list = new InsnList();
            //get world instance
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "portablejim/bbw/core/WandWorker", "world", "Lportablejim/bbw/shims/IWorldShim;"));
            list.add(new MethodInsnNode(INVOKEINTERFACE, "portablejim/bbw/shims/IWorldShim", "getWorld", "()Lnet/minecraft/world/World;", true));
            //block to place
            list.add(new VarInsnNode(ALOAD, 2));
            //block pos
            list.add(new TypeInsnNode(NEW, "net/minecraft/util/math/BlockPos"));
            list.add(new InsnNode(DUP));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new FieldInsnNode(GETFIELD, "portablejim/bbw/basics/Point3d", "x", "I"));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new FieldInsnNode(GETFIELD, "portablejim/bbw/basics/Point3d", "y", "I"));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new FieldInsnNode(GETFIELD, "portablejim/bbw/basics/Point3d", "z", "I"));
            list.add(new MethodInsnNode(INVOKESPECIAL, "net/minecraft/util/math/BlockPos", "<init>", "(III)V", false));
            //add additional params
            list.add(new InsnNode(ICONST_0));
            list.add(new VarInsnNode(ALOAD, 4));
            //placer
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "portablejim/bbw/core/WandWorker", "player", "Lportablejim/bbw/shims/IPlayerShim;"));
            list.add(new MethodInsnNode(INVOKEINTERFACE, "portablejim/bbw/shims/IPlayerShim", "getPlayer", "()Lnet/minecraft/entity/player/EntityPlayer;", true));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_190527_a" : "mayPlace", "(Lnet/minecraft/block/Block;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraft/util/EnumFacing;Lnet/minecraft/entity/Entity;)Z", false));
            //remove '!' from check
            ((JumpInsnNode)insn.getNext()).setOpcode(IFEQ);
            //add new & remove old
            instructions.insert(insn, list);
            removeFrom(instructions, insn, -3);
            return true;
        }

        return false;
    }
}

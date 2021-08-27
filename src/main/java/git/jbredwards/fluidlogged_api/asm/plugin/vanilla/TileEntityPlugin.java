package git.jbredwards.fluidlogged_api.asm.plugin.vanilla;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * allows stored tile entities to be recreated again when the world loads
 * @author jbred
 *
 */
public class TileEntityPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //getBlockMetadata
        if(ASMUtils.checkMethod(method, obfuscated ? "func_145832_p" : "getBlockMetadata", "()I")) {
            currentMethod = 1;
            return true;
        }
        //markDirty
        if(ASMUtils.checkMethod(method, "markDirty", "()V")) {
            currentMethod = 1;
            return true;
        }
        //getBlockType
        if(ASMUtils.checkMethod(method, obfuscated ? "func_145838_q" : "getBlockType", "()Lnet/minecraft/block/Block;")) {
            currentMethod = 1;
            return true;
        }
        //getUpdatePacket
        if(ASMUtils.checkMethod(method, obfuscated ? "func_189518_D_" : "getUpdatePacket", "()Lnet/minecraft/network/play/server/SPacketUpdateTileEntity;")) {
            currentMethod = 2;
            return true;
        }
        //receiveClientEvent
        if(ASMUtils.checkMethod(method, obfuscated ? "func_145842_c" : "receiveClientEvent", null)) {
            method.maxStack += 3;
            currentMethod = 3;
            return true;
        }
        //onDataPacket
        if(ASMUtils.checkMethod(method, "onDataPacket", null)) {
            method.maxStack += 2;
            currentMethod = 4;
            return true;
        }
        //getRenderBoundingBox
        if(ASMUtils.checkMethod(method, "getRenderBoundingBox", "()Lnet/minecraft/util/math/AxisAlignedBB;")) {
            currentMethod = 1;
            return true;
        }

        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //change getBlockState to getStoredOrReal
        if(currentMethod == 1 && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
        }
        //getUpdatePacket, line 221
        if(currentMethod == 2 && insn.getOpcode() == ACONST_NULL) {
            instructions.insert(method("getUpdatePacket", "(Lnet/minecraft/tileentity/TileEntity;)Lnet/minecraft/network/play/server/SPacketUpdateTileEntity;"));
            instructions.insert(new VarInsnNode(ALOAD, 0));
            instructions.remove(insn);
            return true;
        }
        //receiveClientEvent, line 252
        if(currentMethod == 3 && insn.getOpcode() == ICONST_0) {
            final InsnList list = new InsnList();
            //TileEntity param
            list.add(new VarInsnNode(ALOAD, 0));
            //id param
            list.add(new VarInsnNode(ILOAD, 1));
            //type param
            list.add(new VarInsnNode(ILOAD, 2));
            //add new code
            list.add(method("receiveClientEvent", "(Lnet/minecraft/tileentity/TileEntity;II)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        //onDataPacket, line 349
        if(currentMethod == 4 && insn.getOpcode() == RETURN) {
            final InsnList list = new InsnList();
            //TileEntity param
            list.add(new VarInsnNode(ALOAD, 0));
            //SPacketUpdateTileEntity param
            list.add(new VarInsnNode(ALOAD, 2));
            //add new code
            list.add(method("onDataPacket", "(Lnet/minecraft/tileentity/TileEntity;Lnet/minecraft/network/play/server/SPacketUpdateTileEntity;)V"));
            instructions.insert(insn.getPrevious(), list);
            return true;
        }

        return false;
    }

    /*@Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //line 131
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_145839_a" : "readFromNBT", null)) {
            final InsnList list = new InsnList();
            //TileEntity local var
            list.add(new VarInsnNode(ALOAD, 2));
            //TileEntity local var (for new method param)
            list.add(new VarInsnNode(ALOAD, 2));
            //World param
            list.add(new VarInsnNode(ALOAD, 0));
            //NBTTagCompound param
            list.add(new VarInsnNode(ALOAD, 1));
            //method
            list.add(method("create", "(Lnet/minecraft/tileentity/TileEntity;Lnet/minecraft/world/World;Lnet/minecraft/nbt/NBTTagCompound;)Lnet/minecraft/tileentity/TileEntity;"));
            //set the TileEntity local var equal to the stored tile entity
            list.add(new VarInsnNode(ASTORE, 2));
            //add the new code
            instructions.insert(insn, list);
            return true;
        }

        return false;
    }*/
}

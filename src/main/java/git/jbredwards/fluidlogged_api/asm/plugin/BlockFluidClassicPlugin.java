package git.jbredwards.fluidlogged_api.asm.plugin;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * -allows fluidlogged parent blocks to connect with non-fluidlogged fluids (if they are of the same type)
 * -fluids no longer flow from sides they shouldn't
 * @author jbred
 *
 */
public class BlockFluidClassicPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //getQuantaValue
        if(ASMUtils.checkMethod(method, "getQuantaValue", null)) {
            currentMethod = 1;
            return true;
        }
        //updateTick
        if(ASMUtils.checkMethod(method, obfuscated ? "func_180650_b" : "updateTick", null)) {
            currentMethod = 2;
            return true;
        }
        //isSourceBlock
        if(ASMUtils.checkMethod(method, "isSourceBlock", null)) {
            currentMethod = 3;
            return true;
        }

        //default
        return false;
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //getQuantaValue, line 94
        if(currentMethod == 1 && insn.getOpcode() == ICONST_M1) {
            final InsnList list = new InsnList();
            //fluid block variable
            list.add(new VarInsnNode(ALOAD, 0));
            //iblockstate variable
            list.add(new VarInsnNode(ALOAD, 3));
            //quantaPerBlock variable
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "quantaPerBlock", "I"));
            //actually adds the new code
            list.add(method("getQuantaValue", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/block/state/IBlockState;I)I"));

            instructions.insert(insn, list);
            instructions.remove(insn);

            return true;
        }
        //updateTick, line 146
        if(currentMethod == 2 && insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, "getLargerQuanta", null)) {
            method.maxStack += 2;

            final InsnList list = new InsnList();
            //EnumFacing local var
            list.add(new VarInsnNode(ALOAD, 10));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //quantaPerBlock var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "quantaPerBlock", "I"));
            //densityDir ver
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "densityDir", "I"));
            //method
            list.add(method("getLargerQuanta", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ILnet/minecraft/util/EnumFacing;Lnet/minecraftforge/fluids/Fluid;II)I"));

            instructions.insert(insn, list);
            instructions.remove(insn);

            return true;
        }
        //isSourceBlock, line 213
        if(currentMethod == 3 && insn.getOpcode() == INVOKEINTERFACE && ASMUtils.checkMethod(insn, obfuscated ? "func_177230_c" : "getBlock", null)) {
            method.maxStack += 2;

            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //IBlockAccess param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));

            list.add(method("isSourceBlock", "(Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/Block;"));
            instructions.insert(insn, list);

            finishedAll = true;
            return true;
        }

        return false;
    }
}

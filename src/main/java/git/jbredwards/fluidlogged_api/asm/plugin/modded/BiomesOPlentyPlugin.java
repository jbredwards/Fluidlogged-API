package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 * fixes weird fluidlogged fluid block interactions
 * @author jbred
 *
 */
public class BiomesOPlentyPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) {
        return "checkForMixing";
    }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, "func_177230_c", null) || ASMUtils.checkMethod(insn, "getBlock", null)) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //World param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));
            //EnumFacing local var
            list.add(new VarInsnNode(ALOAD, 8));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //method
            list.add(method("BOPCompat", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/fluids/Fluid;)Lnet/minecraft/block/Block;"));

            instructions.insert(insn, list);
            instructions.remove(insn);
            method.maxStack += 3;
            return true;
        }

        return false;
    }
}

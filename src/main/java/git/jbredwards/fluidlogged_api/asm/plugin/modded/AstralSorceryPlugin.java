package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes weird fluidlogged fluid block interactions
 * @author jbred
 *
 */
public class AstralSorceryPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "interactWithAdjacent";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, "func_76224_d", "()Z") || ASMUtils.checkMethod(insn, "isLiquid", "()Z")) {
            final InsnList list = new InsnList();
            //World param
            list.add(new VarInsnNode(ALOAD, 1));
            //BlockPos param
            list.add(new VarInsnNode(ALOAD, 2));
            //EnumFacing local var
            list.add(new VarInsnNode(ALOAD, 8));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));

            //adds new code
            list.add(method("ASCompat", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/fluids/Fluid;)Z"));
            instructions.insert(insn, list);
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

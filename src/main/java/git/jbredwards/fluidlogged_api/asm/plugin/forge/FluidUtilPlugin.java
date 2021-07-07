package git.jbredwards.fluidlogged_api.asm.plugin.forge;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public class FluidUtilPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "tryPlaceFluid";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/capability/IFluidHandler;Lnet/minecraftforge/fluids/FluidStack;)Z";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //tryPlaceFluid, line 639
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_176200_f" : "isReplaceable", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z")) {
            final InsnList list = new InsnList();
            //IBlockState local var
            list.add(new VarInsnNode(ALOAD, 6));
            //Fluid local var
            list.add(new VarInsnNode(ALOAD, 5));
            //method
            list.add(method("isReplaceable", "(ZLnet/minecraft/block/state/IBlockState;Lnet/minecraftforge/fluids/Fluid;)Z"));
            instructions.insert(insn, list);
            return true;
        }

        return false;
    }
}

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
public class FluidBlockWrapperPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "fill";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraftforge/fluids/FluidStack;Z)I";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //place, line 70
        if(ASMUtils.checkMethod(insn, "place", null)) {
            instructions.insert(insn, method("placeModded", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;Z)I"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

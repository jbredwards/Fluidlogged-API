package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * changes some of this class's util functions to be FluidState sensitive
 * @author jbred
 *
 */
public final class PluginFluidUtil implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) {
        return checkMethod(method, "tryPlaceFluid", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/capability/IFluidHandler;Lnet/minecraftforge/fluids/FluidStack;)Z");
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(index == 1) {
            if(checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock", null)) {
                final InsnList list = new InsnList();
                //Fluid local var
                list.add(new VarInsnNode(ALOAD, 5));
                //IBlockState local var
                list.add(new VarInsnNode(ALOAD, 6));
                //add new code
                list.add(genMethodNode("tryPlaceFluid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/state/IBlockState;)Z"));
                instructions.insertBefore(insn, list);
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }
}

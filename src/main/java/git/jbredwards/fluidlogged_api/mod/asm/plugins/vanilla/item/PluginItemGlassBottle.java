package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * glass bottles can now be filled by using water FluidStates
 * @author jbred
 *
 */
public final class PluginItemGlassBottle implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            ((JumpInsnNode)getNext(insn, 3)).setOpcode(IFEQ);
            final InsnList list = new InsnList();
            list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            list.add(new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", false));
            list.add(new FieldInsnNode(GETSTATIC, "net/minecraftforge/fluids/FluidRegistry", "WATER", "Lnet/minecraftforge/fluids/Fluid;"));
            list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "isCompatibleFluid", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraftforge/fluids/Fluid;)Z"));

            instructions.insertBefore(insn, list);
            removeFrom(instructions, insn, 2);
            return true;
        }

        return false;
    }
}

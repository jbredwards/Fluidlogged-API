package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * wooden bowls account for FluidStates when filled
 * @author jbred
 *
 */
public final class PluginGardenOfGlass implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("onPlayerInteract"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onPlayerInteract:
         * Old code:
         * if (event.getWorld().getBlockState(rtr.getBlockPos()).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         *
         * New code:
         * //
         * if (FluidloggedUtils.getFluidOrReal(event.getWorld(), rtr.getBlockPos()).getMaterial() == Material.WATER)
         * {
         *     ...
         * }
         */
        if(checkMethod(insn.getNext(), obfuscated ? "func_185904_a" : "getMaterial")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

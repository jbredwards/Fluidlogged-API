package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allow "fluid on top" barrel crafting to accept FluidStates
 * @author jbred
 *
 */
public final class PluginExNihiloCreatio implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("update"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * update:
         * Old code:
         * Block onTop = barrel.getWorld().getBlockState(pos).getBlock();
         *
         * New code:
         * //use FluidState if it can flow down
         * Block onTop = Hooks.getConnectedFluidOrReal(barrel.getWorld(), pos);
         */
        if(checkMethod(insn, obfuscated ? "func_177230_c" : "getBlock")) {
            instructions.insert(insn, genMethodNode("getConnectedFluidOrReal", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/Block;"));
            removeFrom(instructions, insn, -1);
        }
        /*
         * update:
         * Old code:
         * fluidOnTop = onTop.getMaterial(barrel.getWorld().getBlockState(pos)) == Material.WATER
         *
         * New code:
         * //skip using the state from the world, since it would be incorrect for fluidlogged blocks
         * fluidOnTop = onTop.getDefaultState().getMaterial() == Material.WATER
         */
        else if(checkMethod(insn, obfuscated ? "func_149688_o" : "getMaterial")) {
            instructions.insert(insn, new MethodInsnNode(INVOKEINTERFACE, "net/minecraft/block/state/IBlockState", obfuscated ? "func_185904_a" : "getMaterial", "()Lnet/minecraft/block/material/Material;", true));
            instructions.insert(insn, new MethodInsnNode(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false));
            removeFrom(instructions, insn, -4);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Block getConnectedFluidOrReal(@Nonnull World world, BlockPos pos) {
            final IBlockState here = world.getBlockState(pos);
            return FluidloggedUtils.isFluid(here) || here.getBlock().isAir(here, world, pos)
                    || !FluidloggedUtils.canFluidFlow(world, pos, here, EnumFacing.DOWN)
                    ? here.getBlock() : FluidState.get(world, pos).getBlock();
        }
    }
}

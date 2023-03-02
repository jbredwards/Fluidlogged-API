package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.item;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.PluginBlockLilyPad;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * lily pads can be placed on certain water FluidStates
 * @author jbred
 *
 */
public final class PluginItemLilyPad implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_77659_a" : "onItemRightClick"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * onItemRightClick: (changes are around line 52)
         * Old code:
         * IBlockState iblockstate = worldIn.getBlockState(pos.down());
         *
         * New code:
         * //use the FluidState if the state below is less than 1 block tall
         * IBlockState iblockstate = Hooks.getFluidIfApplicable(worldIn, pos.down());
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("getFluidIfApplicable", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static IBlockState getFluidIfApplicable(@Nonnull World world, @Nonnull BlockPos pos) {
            return PluginBlockLilyPad.Hooks.getFluidIfApplicable(world, pos);
        }
    }
}

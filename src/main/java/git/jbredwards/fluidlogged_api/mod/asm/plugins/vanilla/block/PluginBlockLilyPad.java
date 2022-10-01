package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * lily pads can stay on certain water FluidStates
 * @author jbred
 *
 */
public final class PluginBlockLilyPad implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_180671_f" : "canBlockStay"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * canBlockStay (changes are around line 67):
         * Old code:
         * IBlockState iblockstate = worldIn.getBlockState(pos.down());
         *
         * New code:
         * //use the FluidState if the state below is less than 1 block tall
         * IBlockState iblockstate = Hooks.getFluidIfApplicable(worldIn, pos.down());
         */
        if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("getFluidIfApplicable", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static IBlockState getFluidIfApplicable(@Nonnull World world, @Nonnull BlockPos pos) {
            final IBlockState state = world.getBlockState(pos);
            return state.getBoundingBox(world, pos).maxY < 1 ?
                    FluidloggedUtils.getFluidOrReal(world, pos, state) : state;
        }
    }
}

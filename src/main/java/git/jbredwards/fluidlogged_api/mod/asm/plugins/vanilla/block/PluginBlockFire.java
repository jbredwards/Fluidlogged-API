package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.properties.PropertyInteger;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fire doesn't destroy fluidlogged fluids
 * @author jbred
 *
 */
public final class PluginBlockFire implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("tryCatchFire"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * tryCatchFire: (changes are around line 312)
         * Old code:
         * worldIn.setBlockState(pos, this.getDefaultState().withProperty(AGE, Integer.valueOf(j)), 3);
         *
         * New code:
         * //fire doesn't destroy fluidlogged fluids
         * worldIn.setBlockState(pos, Hooks.getFireOrFluid(this.getDefaultState(), AGE, Integer.valueOf(j), worldIn, pos), 3);
         */
        if(checkMethod(insn, obfuscated ? "func_177226_a" : "withProperty")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(genMethodNode("getFireOrFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/block/properties/PropertyInteger;Ljava/lang/Integer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static IBlockState getFireOrFluid(@Nonnull IBlockState fire, @Nonnull PropertyInteger ageProp, @Nonnull Integer newAge, @Nonnull World world, @Nonnull BlockPos pos) {
            final FluidState fluidState = FluidState.get(world, pos);
            return fluidState.isEmpty() ? fire.withProperty(ageProp, newAge) : fluidState.getState();
        }
    }
}

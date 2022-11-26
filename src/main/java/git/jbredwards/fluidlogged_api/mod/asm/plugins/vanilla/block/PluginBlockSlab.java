package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.BlockSlab;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * makes slabs fluidloggable by default
 * @author jbred
 *
 */
public final class PluginBlockSlab implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        /*
         * isFluidloggable:
         * New code:
         * //fluidloggable if not a double slab
         * @ASMGenerated
         * public boolean isFluidloggable(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos)
         * {
         *     return Hooks.isSlabFluidloggable(this);
         * }
         */
        addMethod(classNode, "isFluidloggable", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "isSlabFluidloggable", "(Lnet/minecraft/block/BlockSlab;)Z", generator -> generator.visitVarInsn(ALOAD, 0)
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isSlabFluidloggable(@Nonnull BlockSlab slab) { return !slab.isDouble(); }
    }
}

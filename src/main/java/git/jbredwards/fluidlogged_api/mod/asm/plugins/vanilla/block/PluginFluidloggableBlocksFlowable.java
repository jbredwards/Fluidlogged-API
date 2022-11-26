package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * for some fluidloggable blocks it makes sense to have the fluid flow from any side
 * @author jbred
 *
 */
public final class PluginFluidloggableBlocksFlowable implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggable");
        /*
         * canFluidFlow:
         * New code:
         * //let fluids flow from any side
         * @ASMGenerated
         * public boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side)
         * {
         *     return true;
         * }
         */
        addMethod(classNode, "canFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", null, null,
                generator -> generator.visitInsn(ICONST_1));

        return false;
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
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
        addMethod(classNode, "canFluidFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", null, null, generator -> {
            generator.visitInsn(ICONST_1);
            generator.visitMaxs(1, 0);
        });

        return false;
    }
}

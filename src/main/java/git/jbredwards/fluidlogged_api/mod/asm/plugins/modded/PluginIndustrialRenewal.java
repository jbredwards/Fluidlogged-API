package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.Block;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * Fix industrial renewal mod's multi-blocks
 * @author jbred
 *
 */
public final class PluginIndustrialRenewal implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_176196_c" : "canPlaceBlockAt"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * canPlaceBlockAt:
         * Old code:
         * this.isReplaceable(worldIn, pos)
         *
         * New code:
         * //fix industrial renewal mod's multi-blocks
         * Hooks.isReplaceable(this, worldIn, pos)
         */
        if(checkMethod(insn, obfuscated ? "func_176200_f" : "isReplaceable")) {
            instructions.insert(insn, genMethodNode("isReplaceable", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isReplaceable(@Nonnull Block unused, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            return world.getBlockState(pos).getBlock().isReplaceable(world, pos);
        }
    }
}

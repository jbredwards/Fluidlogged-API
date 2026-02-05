package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.schematica;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

/**
 * Don't use chunks when getting IFluidHandler
 * @author jbred
 *
 */
public final class PluginBlockList implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull final MethodNode method, final boolean obfuscated) { return method.name.equals("getList"); }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        /*
         * Old code:
         * IFluidHandler fluidHandler = FluidUtil.getFluidHandler(world, pos, null);
         *
         * New code:
         * // Don't use chunks when getting IFluidHandler.
         * IFluidHandler fluidHandler = PluginFluidUtil.Hooks.getFluidStateHandler(world, pos, null);
         */
        if(checkMethod(insn, "getFluidHandler")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginFluidUtil$Hooks", "getFluidStateHandler", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/capability/IFluidHandler;"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block.PluginBlockWall;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * allow wires to connect through fluids
 * @author jbred
 *
 */
public final class PluginProjectRed implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("outsideCornerEdgeOpen"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * outsideCornerEdgeOpen:
         * Old code: (scala)
         * if (world.isAirBlock(pos)) true
         *
         * New code: (scala)
         * //allow wires to connect through fluids
         * if (Hooks.isAirOrFluid(world, pos)) true
         */
        if(checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock")) {
            instructions.insert(insn, genMethodNode("isAirOrFluid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isAirOrFluid(@Nonnull World world, @Nonnull BlockPos pos) {
            return PluginBlockWall.Hooks.isAirOrFluid(world, pos);
        }
    }
}

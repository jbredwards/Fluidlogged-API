package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.ASMUtils;
import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 * fixes fluidlogged trapdoor interaction
 * @author jbred
 *
 */
public final class BlockTrapDoorPlugin extends AbstractPlugin
{
    @Override
    protected boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //onBlockActivated
        final String obaName = obfuscated ? "func_180639_a" : "onBlockActivated";
        //neighborChanged
        final String ncName = obfuscated ? "func_189546_a" : "neighborChanged";
        //if method is either
        return method.name.equals(obaName) || method.name.equals(ncName);
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, null, "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z")) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/BlockTrapDoorPlugin", "set", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z", false));
            instructions.remove(insn);
        }

        return false;
    }

    //unused
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "";
    }

    //unused
    @Nonnull
    @Override
    public String getMethodDesc() {
        return "";
    }

    //boolean value is unused, but exists to prevent crash
    @SuppressWarnings("unused")
    public static boolean set(World world, BlockPos pos, IBlockState state, int ignored) {
        FluidloggedUtils.setStoredOrReal(world, pos, world.getBlockState(pos), state, true);
        return true;
    }
}

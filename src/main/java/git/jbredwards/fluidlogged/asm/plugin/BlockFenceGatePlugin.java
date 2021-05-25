package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.ASMUtils;
import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 * allows fence gates to not have glitched behavior when fluidlogged
 * @author jbred
 *
 */
public final class BlockFenceGatePlugin extends AbstractPlugin
{
    @Override
    protected boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //almost half of the methods must get changed in this class, wow
        return true;
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //world.getBlockState to FluidloggedUtils.getStoredOrReal
        if(insn.getOpcode() == INVOKEINTERFACE && ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
        }
        //world.setBlockState to BlockFenceGatePlugin.set
        else if(insn.getOpcode() == INVOKEVIRTUAL && ASMUtils.checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/BlockFenceGatePlugin", "set", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z", false));
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

    @SuppressWarnings("unused")
    public static boolean set(World world, BlockPos pos, IBlockState state, int flags) {
        final TileEntity te = world.getTileEntity(pos);

        //fluidlogged
        if(te instanceof TileEntityFluidlogged) {
            ((TileEntityFluidlogged)te).setStored(state, true);
            return true;
        }

        //default
        return world.setBlockState(pos, state, flags);
    }
}

package git.jbredwards.fluidlogged.asm.plugin;

import com.google.common.collect.Lists;
import git.jbredwards.fluidlogged.asm.ASMUtils;
import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.common.block.AbstractFluidloggedBlock;
import net.minecraft.block.BlockSponge;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

import java.util.Queue;

import static org.objectweb.asm.Opcodes.*;

/**
 * sponges now drain fluids properly
 * @author jbred
 *
 */
public final class BlockSpongePlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_176311_e" : "tryAbsorb";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)V";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKESPECIAL && ASMUtils.checkMethod(insn, obfuscated ? "func_176312_d" : "absorb", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/BlockSpongePlugin", "absorb", "(Lnet/minecraft/block/BlockSponge;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", false));
            instructions.remove(insn);

            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static boolean absorb(BlockSponge sponge, World world, BlockPos posIn) {
        final Queue<Pair<BlockPos, Integer>> queue = Lists.newLinkedList();
        queue.add(Pair.of(posIn, 0));

        int blocksDrained = 0;

        while(!queue.isEmpty()) {
            Pair<BlockPos, Integer> pair = queue.poll();
            BlockPos pos = pair.getLeft();
            int distance = pair.getRight();

            for(EnumFacing facing : EnumFacing.values()) {
                BlockPos offset = pos.offset(facing);
                IBlockState state = world.getBlockState(offset);

                if(state.getMaterial() == Material.WATER) {
                    //custom drain action
                    if(state.getBlock() instanceof AbstractFluidloggedBlock) ((AbstractFluidloggedBlock)state.getBlock()).drain(world, offset, true);
                    else world.setBlockState(offset, Blocks.AIR.getDefaultState());

                    ++blocksDrained;

                    if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                }
            }

            if(blocksDrained > 64) break;
        }

        return blocksDrained > 0;
    }
}

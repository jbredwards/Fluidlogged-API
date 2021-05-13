package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.common.block.IParticleColor;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.color.BlockColors;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MethodNode;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.INVOKEVIRTUAL;


/**
 * applies the new IParticleColor interface
 * @author jbred
 *
 */
public final class ParticleDiggingPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return obfuscated ? "func_187154_b" : "multiplyColor";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/util/math/BlockPos;)V";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKEVIRTUAL && insn instanceof MethodInsnNode && ((MethodInsnNode)insn).owner.equals("net/minecraft/client/renderer/color/BlockColors")) {
            //adds the new code
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/ParticleDiggingPlugin", "getColor",
                    "(Lnet/minecraft/client/renderer/color/BlockColors;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)I", false));
            //removes the old code
            instructions.remove(insn);

            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    public static int getColor(BlockColors blockColors, IBlockState state, World world, BlockPos pos, int index) {
        if(state.getBlock() instanceof IParticleColor) return ((IParticleColor)state.getBlock()).getParticleColor(state, world, pos);
        else return blockColors.colorMultiplier(state, world, pos, index);
    }
}

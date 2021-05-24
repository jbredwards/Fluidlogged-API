package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.util.FluidloggedConstants;
import net.minecraft.client.renderer.BlockModelShapes;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 * registers this mod's built-in models
 * @author jbred
 *
 */
public final class BlockModelShapesPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "<init>";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/client/renderer/block/model/ModelManager;)V";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(insn.getOpcode() == INVOKESPECIAL && insn instanceof MethodInsnNode && ((MethodInsnNode)insn).owner.equals("net/minecraft/client/renderer/BlockModelShapes")) {
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/BlockModelShapesPlugin", "registerBuiltinBlocks", "(Lnet/minecraft/client/renderer/BlockModelShapes;)V", false));

            instructions.insert(insn, list);
        }

        return false;
    }

    @SuppressWarnings("unused")
    @SideOnly(Side.CLIENT)
    public static void registerBuiltinBlocks(BlockModelShapes shapes) {
        for(BlockFluidloggedTE block : FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.values()) {
            //makes sure to not register the vanilla ones
            if(block.fluid != FluidRegistry.WATER && block.fluid != FluidRegistry.LAVA) {
                shapes.registerBuiltInBlocks(block);
            }
        }
    }
}

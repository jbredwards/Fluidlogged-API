package git.jbredwards.fluidlogged.asm.plugin;

import git.jbredwards.fluidlogged.asm.AbstractPlugin;
import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged.util.FluidloggedConstants;
import net.minecraft.block.Block;
import net.minecraft.util.BlockRenderLayer;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import org.apache.logging.log4j.Logger;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static org.objectweb.asm.Opcodes.*;

/**
 *
 * @author jbred
 *
 */
public final class FluidPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "setBlock";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/block/Block;)Lnet/minecraftforge/fluids/Fluid;";
    }

    @Override
    protected boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        //registers a fluid-logged block for each fluid with a block
        if(insn.getOpcode() == PUTFIELD) {
            //adds code below line 188 in the Fluid class
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //fluid variable
            list.add(new VarInsnNode(ALOAD, 0));
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/FluidPlugin", "registerFluidloggedBlock", "(Lnet/minecraftforge/fluids/Fluid;)V", false));

            instructions.insert(insn, list);
        }
        //fixes console spam
        if(insn.getOpcode() == INVOKEINTERFACE) {
            //adds code at line 192 in the Fluid class
            final InsnList list = new InsnList();
            list.add(new LabelNode());
            //actually adds the new code
            list.add(new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged/asm/plugin/FluidPlugin", "fluidBlockErrorSpamFix", "(Lorg/apache/logging/log4j/Logger;Ljava/lang/String;Lnet/minecraft/block/Block;Ljava/lang/String;Lnet/minecraft/block/Block;)V", false));

            instructions.insert(insn, list);
            instructions.remove(insn);

            return true;
        }

        return false;
    }

    //when a block binds to a fluid, this registers a new waterlogged block
    //I tried FluidRegisterEvent, but that didn't work since it usually gets called prior to the fluid getting bound to its block
    @SuppressWarnings("unused")
    public static void registerFluidloggedBlock(Fluid fluid) {
        if(fluid.getName() != null && FluidRegistry.isFluidRegistered(fluid) && fluid.getBlock() instanceof BlockFluidClassic && !FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.containsKey(fluid)) {
            //generates the block
            final BlockFluidloggedTE block = new BlockFluidloggedTE(fluid, fluid.getBlock().getDefaultState().getMaterial());
            //stores the block
            FluidloggedConstants.FLUIDLOGGED_TE_LOOKUP.put(fluid, block);

            //registers the block
            ForgeRegistries.BLOCKS.register(block.setRegistryName(fluid.getName() + "logged_te").setUnlocalizedName(fluid.getBlock().getUnlocalizedName().replaceFirst("tile.", "")));
        }
    }

    //empty function that replaces the forge error that occurs when a fluid attempts to bind a second block to itself
    @SuppressWarnings("unused")
    public static void fluidBlockErrorSpamFix(Logger logger, String message, Block block, String fluidName, Block old) {}
}

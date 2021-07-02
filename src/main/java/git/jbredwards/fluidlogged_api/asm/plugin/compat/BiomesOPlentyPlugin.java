package git.jbredwards.fluidlogged_api.asm.plugin.compat;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * compat for the biomes o plenty mod
 * @author jbred
 *
 */
public class BiomesOPlentyPlugin extends AbstractPlugin
{
    @Nonnull
    @Override
    public String getMethodName(boolean obfuscated) {
        return "checkForMixing";
    }

    @Nonnull
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_177230_c" : "getBlock", "()Lnet/minecraft/block/Block;")) {
            final InsnList list = new InsnList();
            //this param
            list.add(new VarInsnNode(ALOAD, 0));
            //definedFluid var
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            //method
            list.add(method("BOPCompat", "(Lnet/minecraft/block/Block;Lnet/minecraft/block/Block;Lnet/minecraftforge/fluids/Fluid;)Lnet/minecraft/block/Block;"));

            instructions.insert(insn, list);
            method.maxStack += 3;
            return true;
        }

        return false;
    }
}

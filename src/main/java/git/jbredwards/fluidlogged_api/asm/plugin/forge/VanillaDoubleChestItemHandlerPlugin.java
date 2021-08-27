package git.jbredwards.fluidlogged_api.asm.plugin.forge;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public class VanillaDoubleChestItemHandlerPlugin extends AbstractPlugin
{
    @Nullable
    @Override
    public String getMethodName(boolean obfuscated) { return "get"; }

    @Nullable
    @Override
    public String getMethodDesc() {
        return "(Lnet/minecraft/tileentity/TileEntityChest;)Lnet/minecraftforge/items/VanillaDoubleChestItemHandler;";
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return true;
        }

        return false;
    }
}

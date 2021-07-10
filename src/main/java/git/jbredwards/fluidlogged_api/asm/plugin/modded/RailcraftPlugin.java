package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import git.jbredwards.fluidlogged_api.asm.ASMUtils;
import git.jbredwards.fluidlogged_api.asm.AbstractMultiMethodPlugin;
import org.objectweb.asm.tree.*;

/**
 * prevents crashes when railcraft minecarts travel over fluidlogged rails
 * also fixes fluidlogged rail block glitches that railcraft introduces
 * @author jbred
 *
 */
public class RailcraftPlugin extends AbstractMultiMethodPlugin
{
    @Override
    public boolean isMethodValid(MethodNode method, boolean obfuscated) {
        //CartTools::placeCart
        if(ASMUtils.checkMethod(method, "placeCart", null)) return true;
        //EntityCartBasic::moveAlongTrack
        if(ASMUtils.checkMethod(method, "moveAlongTrack", null)) return true;
        if(ASMUtils.checkMethod(method, "func_180460_a", null)) return true;
        //RenderCart::getPosOffset
        if(ASMUtils.checkMethod(method, "getPosOffset", null)) return true;
        //RenderCart::getPos
        if(ASMUtils.checkMethod(method, "getPos", null)) return true;
        //SoundRegistry::getBlockSound
        if(ASMUtils.checkMethod(method, "getBlockSound", null)) return true;
        //TrackTools::setTrackDirection
        if(ASMUtils.checkMethod(method, "setTrackDirection", null)) {
            currentMethod = 1;
            return true;
        }
        //WorldPlugin::getBlockState
        return ASMUtils.checkMethod(method, "getBlockState", null);
    }

    @Override
    public boolean transform(InsnList instructions, MethodNode method, AbstractInsnNode insn, boolean obfuscated) {
        if(ASMUtils.checkMethod(insn, "getBlockState", null) || ASMUtils.checkMethod(insn, "func_180495_p", null)) {
            instructions.insert(insn, new MethodInsnNode(INVOKESTATIC, "git/jbredwards/fluidlogged_api/util/FluidloggedUtils", "getStoredOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false));
            instructions.remove(insn);
            return false;
        }
        //TrackTools::setTrackDirection, line 119
        if(currentMethod == 1 && (ASMUtils.checkMethod(insn, "setBlockState", null) || ASMUtils.checkMethod(insn, "func_175656_a", null))) {
            instructions.insert(insn, method("setStoredOrRealSimple", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Z"));
            instructions.remove(insn);
            finishedAll = true;
            return true;
        }

        return false;
    }
}

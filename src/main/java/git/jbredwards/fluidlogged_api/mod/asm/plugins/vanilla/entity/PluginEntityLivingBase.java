package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix issue#151
 * @author jbred
 *
 */
public final class PluginEntityLivingBase implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_191986_a" : "travel"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * travel: (changes are around lines 2224 & 2264)
         * Old code:
         * if (this.collidedHorizontally && this.isOffsetPositionInLiquid(this.motionX, this.motionY + 0.6000000238418579D - this.posY + d4, this.motionZ))
         * {
         *     this.motionY = 0.30000001192092896D;
         * }
         *
         * New code:
         * //fix issue#151
         * if (this.collidedHorizontally && this.isOffsetPositionInLiquid(this.motionX, this.motionY + 0.1 - this.posY + d4, this.motionZ))
         * {
         *     this.motionY = 0.30000001192092896D;
         * }
         */
        if(insn.getOpcode() == LDC && ((LdcInsnNode)insn).cst.equals(0.6000000238418579)) ((LdcInsnNode)insn).cst = 0.100001;
        return false;
    }
}

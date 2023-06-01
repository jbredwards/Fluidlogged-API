package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * remove max cache size, fixes issues with fluid sides sometimes randomly not rendering
 * TODO: overwrite whole CachingBakedFluid class to make fluid rendering less laggy
 * @author jbred
 *
 */
public final class PluginCachingBakedFluid implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("<clinit>"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * Old code: (changes are around line 126)
         * private final LoadingCache<Long, BakedFluid> modelCache = CacheBuilder.newBuilder().maximumSize(200).build(...);
         *
         * New code:
         * //remove max cache size, fixes issues with fluid sides sometimes randomly not rendering
         * private final LoadingCache<Long, BakedFluid> modelCache = CacheBuilder.newBuilder().build(...);
         */
        if(checkMethod(insn, "maximumSize")) {
            removeFrom(instructions, insn, -1);
            return true;
        }

        return false;
    }
}

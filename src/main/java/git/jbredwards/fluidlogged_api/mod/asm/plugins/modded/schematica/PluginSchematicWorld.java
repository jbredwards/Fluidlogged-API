package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded.schematica;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * No FluidStates in schematics
 * @author jbred
 *
 */
public final class PluginSchematicWorld implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull final ClassNode classNode, final boolean obfuscated) {
        /*
         * New code:
         * // No FluidStates in schematics (TODO: add support for this in the future?)
         * @ASMGenerated
         * public FluidState getFluidState(int x, int y, int z)
         * {
         *     return FluidState.EMPTY;
         * }
         */
        addMethod(classNode, "getFluidState", "(III)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null, generator -> {
            generator.visitFieldInsn(GETSTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidState", "EMPTY", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });

        return false;
    }
}

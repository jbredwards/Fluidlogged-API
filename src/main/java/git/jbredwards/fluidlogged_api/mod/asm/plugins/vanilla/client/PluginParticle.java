package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fix particle lighting while within fluidlogged blocks
 * @author jbred
 *
 */
public final class PluginParticle implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals(obfuscated ? "func_189214_a" : "getBrightnessForRender"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getBrightnessForRender: (changes are around line 370)
         * Old code:
         * return this.world.isBlockLoaded(blockpos) ? this.world.getCombinedLight(blockpos, 0) : 0;
         *
         * New code:
         * //fix particle lighting while within fluidlogged blocks
         * return this.world.isBlockLoaded(blockpos) ? Hooks.fixParticleBrightness(this.world, blockpos) : 0;
         */
        if(checkMethod(insn, obfuscated ? "func_175626_b" : "getCombinedLight")) {
            instructions.insert(insn, genMethodNode("fixParticleBrightness", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I"));
            removeFrom(instructions, insn, -1);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static int fixParticleBrightness(@Nonnull World world, @Nonnull BlockPos pos) {
            final int sky = fixParticleBrightness(world, pos, EnumSkyBlock.SKY);
            final int block = fixParticleBrightness(world, pos, EnumSkyBlock.BLOCK);
            return sky << 20 | Math.max(block, 0) << 4;
        }

        //helper
        public static int fixParticleBrightness(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumSkyBlock type) {
            if(!world.provider.hasSkyLight() && type == EnumSkyBlock.SKY) return 0;
            if(pos.getY() < 0) pos = new BlockPos(pos.getX(), 0, pos.getZ());
            if(!world.isValid(pos)) return type.defaultLightValue;
            if(!world.getBlockState(pos).useNeighborBrightness()) {
                final FluidState fluidState = FluidState.get(pos);
                if(fluidState.isEmpty() || !fluidState.getState().useNeighborBrightness())
                    return world.getLightFor(type, pos);
            }
            return Math.max(world.getLightFor(type, pos.up()),
                    Math.max(world.getLightFor(type, pos.east()),
                            Math.max(world.getLightFor(type, pos.west()),
                                    Math.max(world.getLightFor(type, pos.south()),
                                            world.getLightFor(type, pos.north())))));
        }
    }
}

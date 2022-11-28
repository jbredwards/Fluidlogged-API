package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.biome.BiomeColorHelper;
import net.minecraftforge.fml.common.FMLCommonHandler;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * add water's biome colors to its fluid class
 * @author jbred
 *
 */
public final class PluginFluidWater implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * New code:
         * //use biome here if server, or biome blend if client
         * @ASMGenerated
         * public int getColor(World world, BlockPos pos)
         * {
         *     return Hooks.getWaterColorAt(world, pos);
         * }
         */
        addMethod(classNode, "getColor", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I",
            "getWaterColorAt", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)I", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static int getWaterColorAt(@Nonnull World world, @Nonnull BlockPos pos) {
            return FMLCommonHandler.instance().getSide().isClient()
                    ? BiomeColorHelper.getWaterColorAtPos(world, pos)
                    : world.getBiome(pos).getWaterColorMultiplier();
        }
    }
}

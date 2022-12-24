package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneTorch;
import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLogic;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;

/**
 * allows torches to be destroyed by flowing fluid blocks
 * @author jbred
 *
 */
public final class PluginBlockTorch implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * getMaterial:
         * New code:
         * //allow torches to resist water if the config option is enabled
         * @ASMGenerated
         * public Material getMaterial(IBlockState state)
         * {
         *     return Hooks.getTorchMaterial(this.material, this);
         * }
         */
        addMethod(classNode, obfuscated ? "func_149688_o" : "getMaterial", "(Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/block/material/Material;",
            "getTorchMaterial", "(Lnet/minecraft/block/material/Material;Lnet/minecraft/block/Block;)Lnet/minecraft/block/material/Material;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
                generator.visitVarInsn(ALOAD, 0);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        static final Material TORCH = new MaterialLogic(Material.CIRCUITS.getMaterialMapColor()).setNoPushMobility();

        //PluginBlockTorch
        @Nonnull
        public static Material getTorchMaterial(@Nonnull Material material, @Nonnull Block block) {
            return !FluidloggedAPIConfigHandler.fluidsBreakTorches || block instanceof BlockRedstoneTorch ? material : TORCH;
        }
    }
}

package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.common.config.ConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneTorch;
import net.minecraft.block.BlockTorch;
import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLogic;
import net.minecraft.block.state.IBlockState;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * allows torches to be destroyed by flowing fluid blocks
 * @author jbred
 *
 */
@Mixin(BlockTorch.class)
public abstract class BlockTorchMixin extends Block
{
    private static final Material TORCH = new MaterialLogic(Material.CIRCUITS.getMaterialMapColor()) {
        @Nonnull
        @Override
        public Material setNoPushMobility() { return super.setNoPushMobility(); }
    }.setNoPushMobility();

    public BlockTorchMixin(@Nonnull Material materialIn) { super(materialIn); }

    @Nonnull
    @Override
    public Material getMaterial(@Nonnull IBlockState state) {
        return !ConfigHandler.fluidsBreakTorches || state.getBlock() instanceof BlockRedstoneTorch
                ? super.getMaterial(state) : TORCH;
    }
}

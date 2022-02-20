package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockSlab;
import net.minecraft.block.state.IBlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;

/**
 * makes slabs fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockSlab.class)
public abstract class BlockSlabMixin implements IFluidloggable
{
    @Override
    public boolean isFluidloggable(@Nonnull IBlockState state) { return !isDouble(); }

    @Shadow
    public abstract boolean isDouble();
}

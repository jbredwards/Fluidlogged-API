package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.Block;
import net.minecraft.block.BlockSlab;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;

/**
 * makes slabs fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockSlab.class)
public abstract class BlockSlabMixin extends Block implements IFluidloggable
{
    public BlockSlabMixin(@Nonnull Material materialIn) { super(materialIn); }

    @Shadow
    public abstract boolean isDouble();

    @Override
    public boolean isFluidloggable(@Nonnull IBlockState state) { return !isDouble(); }
}

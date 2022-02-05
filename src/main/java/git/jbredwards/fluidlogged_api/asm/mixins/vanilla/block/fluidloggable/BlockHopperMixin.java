package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.BlockHopper;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * makes hoppers fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockHopper.class)
public abstract class BlockHopperMixin extends BlockContainer implements IFluidloggable
{
    public BlockHopperMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockHopperMixin(@Nonnull Material materialIn) { super(materialIn); }
}

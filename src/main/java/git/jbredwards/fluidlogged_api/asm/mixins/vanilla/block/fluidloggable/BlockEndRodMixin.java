package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockDirectional;
import net.minecraft.block.BlockEndRod;
import net.minecraft.block.material.Material;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * makes end rods fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockEndRod.class)
public abstract class BlockEndRodMixin extends BlockDirectional implements IFluidloggable
{
    public BlockEndRodMixin(@Nonnull Material materialIn) { super(materialIn); }
}

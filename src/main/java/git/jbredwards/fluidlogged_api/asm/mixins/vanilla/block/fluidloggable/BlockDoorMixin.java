package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * makes doors fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockDoor.class)
public abstract class BlockDoorMixin extends Block implements IFluidloggable
{
    public BlockDoorMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockDoorMixin(@Nonnull Material materialIn) { super(materialIn); }
}

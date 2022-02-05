package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.Block;
import net.minecraft.block.BlockTrapDoor;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * makes trapdoors fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockTrapDoor.class)
public abstract class BlockTrapDoorMixin extends Block implements IFluidloggable
{
    public BlockTrapDoorMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockTrapDoorMixin(@Nonnull Material materialIn) { super(materialIn); }
}

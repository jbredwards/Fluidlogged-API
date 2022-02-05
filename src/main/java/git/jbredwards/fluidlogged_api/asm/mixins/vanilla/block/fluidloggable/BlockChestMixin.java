package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockChest;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * makes chests fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockChest.class)
public abstract class BlockChestMixin extends BlockContainer implements IFluidloggable
{
    public BlockChestMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockChestMixin(@Nonnull Material materialIn) { super(materialIn); }
}

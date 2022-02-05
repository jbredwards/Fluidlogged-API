package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.BlockEnderChest;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * makes ender chests fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockEnderChest.class)
public abstract class BlockEnderChestMixin extends BlockContainer implements IFluidloggable
{
    public BlockEnderChestMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockEnderChestMixin(@Nonnull Material materialIn) { super(materialIn); }
}

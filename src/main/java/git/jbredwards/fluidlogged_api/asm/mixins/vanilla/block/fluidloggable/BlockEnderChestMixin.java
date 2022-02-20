package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockEnderChest;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes ender chests fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockEnderChest.class)
public abstract class BlockEnderChestMixin implements IFluidloggable { }

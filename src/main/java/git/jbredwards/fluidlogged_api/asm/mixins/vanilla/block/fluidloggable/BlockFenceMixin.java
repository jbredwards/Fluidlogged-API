package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockFence;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes fences fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockFence.class)
public abstract class BlockFenceMixin implements IFluidloggable { }

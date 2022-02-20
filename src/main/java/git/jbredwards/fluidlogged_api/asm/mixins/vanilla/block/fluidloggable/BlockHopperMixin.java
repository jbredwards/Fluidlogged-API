package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockHopper;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes hoppers fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockHopper.class)
public abstract class BlockHopperMixin implements IFluidloggable { }

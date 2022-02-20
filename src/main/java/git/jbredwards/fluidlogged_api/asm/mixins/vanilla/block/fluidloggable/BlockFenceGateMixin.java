package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockFenceGate;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes fence gates fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockFenceGate.class)
public abstract class BlockFenceGateMixin implements IFluidloggable { }

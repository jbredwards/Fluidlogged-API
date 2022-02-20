package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockDoor;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes doors fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockDoor.class)
public abstract class BlockDoorMixin implements IFluidloggable { }

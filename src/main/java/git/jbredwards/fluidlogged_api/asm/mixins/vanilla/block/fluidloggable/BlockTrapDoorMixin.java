package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockTrapDoor;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes trapdoors fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockTrapDoor.class)
public abstract class BlockTrapDoorMixin implements IFluidloggable { }

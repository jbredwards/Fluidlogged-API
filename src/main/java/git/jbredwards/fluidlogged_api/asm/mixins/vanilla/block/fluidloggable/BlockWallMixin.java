package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockWall;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes walls fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockWall.class)
public abstract class BlockWallMixin implements IFluidloggable { }

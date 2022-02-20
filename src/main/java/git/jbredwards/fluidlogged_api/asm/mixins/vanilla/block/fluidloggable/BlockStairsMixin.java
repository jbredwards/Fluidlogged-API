package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockStairs;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes stairs fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockStairs.class)
public abstract class BlockStairsMixin implements IFluidloggable { }

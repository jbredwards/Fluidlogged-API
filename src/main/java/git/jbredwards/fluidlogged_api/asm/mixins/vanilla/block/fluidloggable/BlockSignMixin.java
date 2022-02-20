package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockSign;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes signs fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockSign.class)
public abstract class BlockSignMixin implements IFluidloggable { }

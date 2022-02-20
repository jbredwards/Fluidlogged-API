package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockPane;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes glass panes fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockPane.class)
public abstract class BlockPaneMixin implements IFluidloggable { }

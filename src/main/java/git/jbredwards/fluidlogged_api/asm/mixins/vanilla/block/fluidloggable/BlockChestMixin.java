package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.BlockChest;
import org.spongepowered.asm.mixin.Mixin;

/**
 * makes chests fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockChest.class)
public abstract class BlockChestMixin implements IFluidloggable { }

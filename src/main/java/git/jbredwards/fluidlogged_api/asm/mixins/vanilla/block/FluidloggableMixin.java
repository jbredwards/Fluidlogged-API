package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.*;
import org.spongepowered.asm.mixin.Mixin;

/**
 * basic fluidlogging implementation for certain blocks
 * (see https://minecraft.fandom.com/wiki/Waterlogging for a rough idea of which ones)
 * @author jbred
 *
 */
@Mixin({BlockAnvil.class, BlockBanner.class, BlockBasePressurePlate.class, BlockBeacon.class, BlockBrewingStand.class,
        BlockButton.class, BlockChest.class, BlockDaylightDetector.class, BlockDoor.class, BlockDragonEgg.class,
        BlockEnderChest.class, BlockEndPortalFrame.class, BlockEndRod.class, BlockFenceGate.class, BlockFence.class,
        BlockFlowerPot.class, BlockHopper.class, BlockLadder.class, BlockLever.class, BlockPane.class,
        BlockRailBase.class, BlockRedstoneDiode.class, BlockSign.class, BlockStairs.class, BlockTrapDoor.class})
public abstract class FluidloggableMixin implements IFluidloggable { }

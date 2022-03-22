package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import net.minecraft.block.*;
import org.spongepowered.asm.mixin.Mixin;

/**
 * basic fluidlogging implementation for certain blocks
 * @author jbred
 *
 */
@Mixin({BlockAnvil.class, BlockBanner.class, BlockBasePressurePlate.class, BlockBeacon.class, BlockBrewingStand.class,
        BlockButton.class, BlockChest.class, BlockDaylightDetector.class, BlockDragonEgg.class, BlockEnderChest.class,
        BlockEndPortalFrame.class, BlockEndRod.class, BlockFenceGate.class, BlockFence.class, BlockFlowerPot.class,
        BlockHopper.class, BlockLadder.class, BlockLever.class, BlockPane.class, BlockPistonExtension.class,
        BlockRailBase.class, BlockRedstoneDiode.class, BlockRedstoneTorch.class, BlockRedstoneWire.class,
        BlockSign.class, BlockStairs.class, BlockTripWire.class, BlockTripWireHook.class})
public abstract class MixinFluidloggableBlocks implements IFluidloggable { }

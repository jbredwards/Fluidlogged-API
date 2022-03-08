package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.BlockTrapDoor;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * makes trap doors fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockTrapDoor.class)
public abstract class BlockTrapDoorMixin implements IFluidloggable
{
    //notify fluids of the state change
    @Redirect(method = {"onBlockActivated", "neighborChanged"}, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
    private boolean setBlockState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, int blockFlags) {
        FluidloggedUtils.notifyFluids(world, pos);
        return world.setBlockState(pos, state, blockFlags);
    }

    //fluids only flow from certain sides
    @Override
    public boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side) {
        final boolean isOpen = here.getValue(BlockTrapDoor.OPEN);

        if(side.getAxis().isHorizontal()) return !isOpen || here.getValue(BlockTrapDoor.FACING).getOpposite() != side;
        else if(side == EnumFacing.UP) return isOpen || here.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.BOTTOM;
        else return isOpen || here.getValue(BlockTrapDoor.HALF) == BlockTrapDoor.DoorHalf.TOP;
    }
}

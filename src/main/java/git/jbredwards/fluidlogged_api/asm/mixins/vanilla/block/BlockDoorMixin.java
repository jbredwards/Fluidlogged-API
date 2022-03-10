package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.BlockDoor;
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
 * makes doors fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockDoor.class)
public abstract class BlockDoorMixin implements IFluidloggable
{
    //ensure the fluids in both parts of the door are updated
    @Redirect(method = {"onBlockActivated", "toggleDoor", "neighborChanged"}, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;markBlockRangeForRenderUpdate(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;)V"))
    private void notifyFluidState(@Nonnull World world, @Nonnull BlockPos rangeMin, @Nonnull BlockPos rangeMax) {
        if(!rangeMin.equals(rangeMax))
            FluidloggedUtils.notifyFluids(world, rangeMin.up(), FluidState.get(world, rangeMin.up()), false, EnumFacing.DOWN);

        world.markBlockRangeForRenderUpdate(rangeMin, rangeMax);
    }

    @Override
    public boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side) {
        if(side.getAxis().isVertical()) return true;

        here = here.getActualState(world, pos);
        final EnumFacing facing = here.getValue(BlockDoor.FACING);

        return (here.getValue(BlockDoor.OPEN) ? (here.getValue(BlockDoor.HINGE) == BlockDoor.EnumHingePosition.RIGHT
                        ? facing.rotateYCCW() : facing.rotateY())
                : facing).getOpposite() != side;
    }
}
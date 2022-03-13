package git.jbredwards.fluidlogged_api.mod.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.wrappers.FluidBlockWrapper;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * helps mods to interact with fluidlogged fluids without a required dependency
 * @author jbred
 *
 */
@Mixin(FluidUtil.class)
public abstract class FluidUtilMixin
{
    /**
     * @reason
     * @author jbred
     */
    @Nullable
    @Overwrite(remap = false)
    public static IFluidHandler getFluidHandler(@Nonnull World world, @Nonnull BlockPos pos, @Nullable EnumFacing side) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().hasTileEntity(state)) {
            final @Nullable TileEntity te = world.getTileEntity(pos);
            if(te != null && te.hasCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, side))
                return te.getCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, side);
        }

        final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, state);
        return fluidState.isEmpty() ? null : new FluidBlockWrapper(fluidState.getBlock(), world, pos);
    }

    @Nonnull
    @Redirect(method = "tryPickUpFluid", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", remap = false), remap = false)
    private static IBlockState getFluidOrReal(@Nonnull World world, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(world, pos); }
}

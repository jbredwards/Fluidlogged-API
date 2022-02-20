package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.common.util.FluidState;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.FluidStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockFluidClassic.class)
public abstract class BlockFluidClassicMixin extends BlockFluidBaseMixin implements IFluidloggableFluid
{
    @Shadow(remap = false)
    protected boolean canCreateSources;

    @Shadow(remap = false)
    protected FluidStack stack;

    public BlockFluidClassicMixin(@Nonnull Material materialIn) { super(materialIn); }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite(remap = false)
    public int getQuantaValue(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        if(!isCompatibleFluid(fluidState.getFluid(), getFluid())) return -1;
        else return quantaPerBlock - fluidState.getLevel();
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluidState) {
        return !hasTileEntity(fluidState) && this == getFluid().getBlock();
    }
}

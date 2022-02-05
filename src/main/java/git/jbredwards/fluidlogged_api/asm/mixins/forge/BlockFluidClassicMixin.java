package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.common.storage.FluidState;
import net.minecraft.block.material.MapColor;
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
@SuppressWarnings({"OverwriteAuthorRequired", "unused"})
@Mixin(BlockFluidClassic.class)
public abstract class BlockFluidClassicMixin extends BlockFluidBaseMixin implements IFluidloggableFluid
{
    @Shadow
    protected boolean canCreateSources;

    @Shadow
    protected FluidStack stack;

    public BlockFluidClassicMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockFluidClassicMixin(@Nonnull Material materialIn) { super(materialIn); }

    @Overwrite
    public int getQuantaValue(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final IBlockState state = world.getBlockState(pos);
        if(state.getBlock().isAir(state, world, pos)) return 0;

        final FluidState fluidState = getFluidState(world, pos, state);
        if(!isCompatibleFluid(fluidState.getFluid(), getFluid())) return -1;
        else return quantaPerBlock - fluidState.getLevel();
    }

    @Override
    public boolean isFluidloggableFluid(@Nonnull IBlockState fluidState) {
        return !fluidState.getBlock().hasTileEntity(fluidState) && getFluid().getBlock() == fluidState.getBlock();
    }
}

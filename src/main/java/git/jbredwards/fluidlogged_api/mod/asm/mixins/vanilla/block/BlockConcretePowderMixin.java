package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockConcretePowder;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;

import javax.annotation.Nonnull;

import static net.minecraft.block.BlockConcretePowder.COLOR;

/**
 * concrete forms from concrete powder while it's next to water FluidStates
 * @author jbred
 *
 */
@Mixin(BlockConcretePowder.class)
public abstract class BlockConcretePowderMixin
{
    /**
     * @reason fixes fluidlogged interaction
     * @author jbred
     */
    @Overwrite
    public void onEndFalling(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState falling, @Nonnull IBlockState replaced) {
        final FluidState fluidState = FluidloggedUtils.getFluidState(worldIn, pos, replaced);
        if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) dry(worldIn, pos, falling);
    }

    /**
     * @reason fixes fluidlogged interaction
     * @author jbred
     */
    @Overwrite
    protected boolean tryTouchWater(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        for(EnumFacing facing : EnumFacing.values()) {
            if(facing != EnumFacing.DOWN) {
                BlockPos offset = pos.offset(facing);
                IBlockState neighbor = worldIn.getBlockState(offset);

                if(FluidloggedUtils.canFluidFlow(worldIn, offset, neighbor, facing.getOpposite())) {
                    FluidState fluidState = FluidloggedUtils.getFluidState(worldIn, offset, neighbor);
                    if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) return dry(worldIn, pos, state);
                }
            }
        }

        return false;
    }

    //shortcut method
    private boolean dry(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        return world.setBlockState(pos, Blocks.CONCRETE.getDefaultState().withProperty(COLOR, state.getValue(COLOR)));
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import net.minecraft.block.BlockLeaves;
import net.minecraft.block.BlockMobSpawner;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;

/**
 * for some fluidloggable blocks it makes sense to have the fluid flow from any side
 * @author jbred
 *
 */
@Mixin({BlockLeaves.class, BlockMobSpawner.class})
public abstract class MixinFluidloggableBlocksFlowable implements IFluidloggable
{
    @Override
    public boolean canFluidFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing side) {
        return true;
    }
}

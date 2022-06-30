package git.jbredwards.fluidlogged_api.mod.common.config.js;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.Immutable;

/**
 *
 * @author jbred
 *
 */
@Immutable
public final class JSWorld
{
    @Nonnull final World world;
    public JSWorld(@Nonnull World worldIn) { world = worldIn; }

    @Nonnull
    public World getJavaWorld() { return world; }

    @Nonnull
    public JSState getState(int x, int y, int z) { return getState(new BlockPos(x, y, z)); }

    @Nonnull
    public JSState getState(@Nullable Object o) {
        if(o instanceof JSPos) return new JSState(world.getBlockState(((JSPos)o).pos));
        else if(o instanceof BlockPos) return new JSState(world.getBlockState((BlockPos)o));
        else return new JSState(Blocks.AIR.getDefaultState());
    }

    @Nonnull
    public JSFluid getFluid(int x, int y, int z) { return getFluid(new BlockPos(x, y, z)); }

    @Nonnull
    public JSFluid getFluid(@Nullable Object o) {
        if(o instanceof JSPos) return new JSFluid(FluidloggedUtils.getFluidState(world, ((JSPos)o).pos));
        else if(o instanceof BlockPos) return new JSFluid(FluidloggedUtils.getFluidState(world, (BlockPos)o));
        else return new JSFluid(FluidState.EMPTY);
    }

    public boolean isRemote() { return world.isRemote; }
}

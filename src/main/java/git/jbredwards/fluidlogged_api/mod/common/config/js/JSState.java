package git.jbredwards.fluidlogged_api.mod.common.config.js;

import net.minecraft.block.Block;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;

import javax.annotation.Nonnull;
import javax.annotation.concurrent.Immutable;

/**
 *
 * @author jbred
 *
 */
@Immutable
public final class JSState
{
    @Nonnull final IBlockState state;
    public JSState(@Nonnull IBlockState stateIn) { state = stateIn; }

    @Nonnull
    public IBlockState getJavaState() { return state; }

    @Nonnull
    public Block getBlock() { return state.getBlock(); }

    public int getMeta() { return getBlock().getMetaFromState(state); }

    public boolean isShapeSolid(@Nonnull JSWorld world, @Nonnull JSPos pos, @Nonnull JSSide side) {
        return state.getBlockFaceShape(world.world, pos.pos, side.facing) == BlockFaceShape.SOLID;
    }

    public boolean isSideSolid(@Nonnull JSWorld world, @Nonnull JSPos pos, @Nonnull JSSide side) {
        return state.isSideSolid(world.world, pos.pos, side.facing);
    }
}

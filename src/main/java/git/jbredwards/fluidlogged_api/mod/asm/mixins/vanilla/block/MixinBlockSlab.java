package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import net.minecraft.block.BlockSlab;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;

/**
 * makes slabs fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockSlab.class)
public abstract class MixinBlockSlab implements IFluidloggable
{
    @Override
    public boolean isFluidloggable(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        return !isDouble();
    }

    @Shadow
    public abstract boolean isDouble();
}

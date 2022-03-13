package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import net.minecraft.block.BlockStaticLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;

import javax.annotation.Nonnull;

/**
 * updates to the dynamic version when added to the world
 * @author jbred
 *
 */
@Mixin(BlockStaticLiquid.class)
public abstract class BlockStaticLiquidMixin extends BlockLiquidMixin
{
    public BlockStaticLiquidMixin(@Nonnull Material materialIn) { super(materialIn); }

    @SuppressWarnings("ConstantConditions")
    @Override
    public void onBlockAdded(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
        //only do this for vanilla blocks, some modded blocks (like BOP coral) extend this class
        if(((Object)this == Blocks.WATER || (Object)this == Blocks.LAVA) && !checkForMixing(worldIn, pos, state))
            updateLiquid(worldIn, pos, state);
    }

    @Shadow
    protected abstract void updateLiquid(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state);
}

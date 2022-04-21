package git.jbredwards.fluidlogged_api.mod.common.util;

import net.minecraft.block.Block;
import net.minecraft.block.BlockBush;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fluids.BlockFluidBase;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * This class is built during runtime via PluginAccessorUtils
 * @author jbred
 *
 */
public final class AccessorUtils
{
    //Block
    @Nullable public static Boolean getCanFluidFlow(@Nonnull Block block) { return null; }
    public static void setCanFluidFlow(@Nonnull Block block, @Nullable Boolean canFluidFlowIn) { }

    //BlockBush
    public static boolean canSustainBush(@Nonnull BlockBush bush, @Nonnull IBlockState state) { return false; }

    //BlockFluidBase
    public static int getFlowDecay(@Nonnull BlockFluidBase block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return 0; }
    public static boolean hasVerticalFlow(@Nonnull BlockFluidBase block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) { return false; }
}

package git.jbredwards.fluidlogged_api.mod.common.util;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.BlockBush;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.gen.structure.template.Template;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.BlockFluidClassic;
import net.minecraftforge.fluids.Fluid;

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

    //BlockFluidClassic
    public static int getLargerQuanta(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare) { return 0; }
    public static int calculateFlowCost(@Nonnull BlockFluidClassic block, @Nonnull World world, @Nonnull BlockPos pos, int recurseDepth, int side) { return 0; }
    public static void flowIntoBlock(@Nonnull BlockFluidClassic block, @Nonnull World world, @Nonnull BlockPos pos, int meta) { }

    //Fluid
    @Nonnull public static FluidState getDefaultFluidState(@Nonnull Fluid fluid) { return null; }
    @Nonnull public static FluidState setDefaultFluidState(@Nonnull Fluid fluid, @Nonnull FluidState fluidState) { return null; }

    //Template
    public static void setKeepOldFluidStates(@Nonnull Template template, boolean keepOldFluidStates) { }
}

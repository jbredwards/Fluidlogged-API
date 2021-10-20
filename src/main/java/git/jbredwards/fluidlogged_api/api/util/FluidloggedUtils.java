package git.jbredwards.fluidlogged_api.api.util;

import git.jbredwards.fluidlogged_api.api.capability.IFluidCapability;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.IFluidBlock;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * helpful functions
 * @author jbred
 *
 */
public enum FluidloggedUtils
{
    ;

    @Nullable
    public static IBlockState getFluidState(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        //if instanceof World, check the chunk capability for a stored fluid
        if(world instanceof World) {
            final Chunk chunk = ((World)world).getChunkFromBlockCoords(pos);
            final @Nullable IBlockState state = convertToFluidState(chunk.getBlockState(pos));
            //get from capability
            if(state == null) {
                final @Nullable IFluidCapability cap = IFluidCapability.get(chunk);
                return cap == null ? null : cap.getFluidState(pos);
            }
            //get from fluid in the world
            return state;
        }
        //default
        else return convertToFluidState(world.getBlockState(pos));
    }

    //converts the inout state into the parent's fluid state (with the level prop carried over)
    //returns null if the state is not a fluid
    @Nullable
    public static IBlockState convertToFluidState(@Nonnull IBlockState state) {
        final @Nullable Fluid fluid = getFluidFromBlock(state.getBlock());
        if(fluid != null) {
            final Block block = fluid.getBlock();
            if(block != null) {
                //if the block here matches the fluid block
                if(block.equals(state.getBlock())
                        || state.getBlock() instanceof BlockLiquid
                        && block.equals(BlockLiquid.getStaticBlock(state.getMaterial()))) return state;
                //fix the level property
                return block.getDefaultState().withProperty(BlockLiquid.LEVEL, state.getValue(BlockLiquid.LEVEL));
            }
        }
        //default
        return null;
    }

    public static boolean setFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nullable IBlockState fluidState, boolean notify, boolean sendToClient) {
        final @Nullable IFluidCapability cap = IFluidCapability.get(world.getChunkFromBlockCoords(pos));
        final @Nullable IBlockState old = cap == null ? null : cap.getFluidState(pos);
        //do nothing if the fluid state won't change
        if(fluidState == old) return false;

        //default
        return true;
    }

    //gets the fluid from the block (null if there is no fluid)
    @Nullable
    public static Fluid getFluidFromBlock(@Nonnull Block fluid) {
        //modded
        if(fluid instanceof IFluidBlock) return ((IFluidBlock)fluid).getFluid();
        //vanilla
        else if(fluid.getDefaultState().getMaterial() == Material.WATER) return FluidRegistry.WATER;
        else return fluid.getDefaultState().getMaterial() == Material.LAVA ? FluidRegistry.LAVA : null;
    }
}

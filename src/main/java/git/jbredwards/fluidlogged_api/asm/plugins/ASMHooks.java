package git.jbredwards.fluidlogged_api.asm.plugins;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggableBase;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.Fluid;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nullable;

import static net.minecraft.util.EnumFacing.*;

/**
 * class exists cause SpongeForge
 * NOTE THAT MOST OF THESE METHODS ARE MEANT TO ONLY BE USED IN CERTAIN CASES,
 * PRIOR TO INTEGRATING THEM TO YOUR OWN MOD, VIEW THE PLUGIN CLASS ASSOCIATED
 * @author jbred
 *
 */
@SuppressWarnings("unused")
public enum ASMHooks
{
    ;

    //=====
    //FORGE
    //=====

    //BlockFluidBasePlugin
    public static boolean isFluid(IBlockState up, Fluid fluid, IBlockAccess world, BlockPos pos) {
        return FluidloggedUtils.getFluidFromBlock(up.getBlock()) == fluid
                && IFluidloggableBase.canFluidFlow(world, pos, up, fluid, DOWN);
    }

    //FluidPlugin
    public static boolean updateIfNotFluidloggable(Block blockOld, Block blockNew) {
        return blockOld == blockNew && !(blockNew instanceof IFluidloggableBase);
    }

    //FluidPlugin
    public static void fluidBlockErrorSpamFix(Logger logger, String message, Block block, String fluidName, Block old) {
        if(!(block instanceof IFluidloggableBase)) logger.warn(message, block, fluidName, old);
    }

    //ModelFluidPlugin
    public static float fixTextureFightingZ(float old, int index) {
        final EnumFacing facing = EnumFacing.getHorizontal((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.X) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //ModelFluidPlugin
    public static float fixTextureFightingX(float old, int index) {
        final EnumFacing facing = EnumFacing.getHorizontal((5 - index) % 4); // [W, S, E, N]
        if(facing.getAxis() == Axis.Z) return old;
        else return old == 1 ? 0.998f : 0.002f;
    }

    //=======
    //VANILLA
    //=======

    //BlockPlugin
    @SuppressWarnings("deprecation")
    public static float getExplosionResistance(Block block, Entity entity, World world, BlockPos pos, Explosion explosion) {
        final float here = block.getExplosionResistance(entity);
        //catch loop
        if(FluidloggedUtils.getFluidFromBlock(block) != null) return here;
        //no fluid here, return old value
        final @Nullable IBlockState fluidState = FluidloggedUtils.getFluidState(world, pos);
        if(fluidState == null) return here;
        //compare the fluid & old values, and return the greater of the two
        else return Math.max(here, fluidState.getBlock().getExplosionResistance(world, pos, entity, explosion));
    }

    //BlockPlugin
    @SuppressWarnings("deprecation")
    public static int getLightOpacity(IBlockState state, IBlockAccess world, BlockPos pos) {
        final int here = state.getLightOpacity();
        //catch loop
        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) != null) return here;
        //no fluid here, return old value
        final @Nullable IBlockState fluidState = FluidloggedUtils.getFluidState(world, pos);
        if(fluidState == null) return here;
        //compare the fluid & old values, and return the greater of the two
        else return Math.max(here, fluidState.getLightOpacity(world, pos));
    }

    //BlockPlugin
    @SuppressWarnings("deprecation")
    public static int getLightValue(IBlockState state, IBlockAccess world, BlockPos pos) {
        final int here = state.getLightValue();
        //catch loop
        if(FluidloggedUtils.getFluidFromBlock(state.getBlock()) != null) return here;
        //no fluid here, return old value
        final @Nullable IBlockState fluidState = FluidloggedUtils.getFluidState(world, pos);
        if(fluidState == null) return here;
        //compare the fluid & old values, and return the greater of the two
        else return Math.max(here, fluidState.getLightValue(world, pos));
    }

    //WorldPlugin
    public static IBlockState setBlockState(Chunk chunk, BlockPos pos, IBlockState state, IBlockState oldState, World world, int flags) {
        final IBlockState ret = chunk.setBlockState(pos, state);
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(state.getBlock());
        if(fluid != null || state.getMaterial() == Material.AIR) {
            if()
        }

        return ret;
    }

    //WorldServerPlugin
    public static IBlockState updateBlockTick(WorldServer world, BlockPos pos, Block compare) {
        final IBlockState here = world.getBlockState(pos);
        //actual block
        if(Block.isEqualTo(here.getBlock(), compare)) return here;
        //fluid
        final @Nullable IBlockState fluidState = FluidloggedUtils.getFluidState(world, pos);
        if(fluidState != null && Block.isEqualTo(here.getBlock(), fluidState.getBlock())) return fluidState;
        //default
        return here;
    }

    //======
    //MODDED
    //======
}

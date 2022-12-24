package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidClassic;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;
import net.minecraft.block.BlockDynamicLiquid;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.IFluidBlock;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * fixes a bunch of liquid interactions while fluidlogged
 * @author jbred
 *
 */
public final class PluginBlockDynamicLiquid implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * updateTick:
         * New code:
         * //rewrite flow logic to work while fluidlogged
         * @ASMGenerated
         * public void updateTick(World worldIn, BlockPos pos, IBlockState state, Random rand)
         * {
         *     Hooks.updateLiquidTick(this, worldIn, pos, state, rand);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "updateLiquidTick", "(Lnet/minecraft/block/BlockDynamicLiquid;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void updateLiquidTick(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
            final int slopeDistance = block.getSlopeFindDistance(world);
            if(!world.isAreaLoaded(pos, slopeDistance)) return;

            final Fluid fluid = FluidloggedUtils.getFluidFromState(state);
            if(fluid == null) throw new IllegalStateException("A critical error has occurred, please issue a bug report!");

            IBlockState here = world.getBlockState(pos); //here state is different than the input one if fluidlogged
            if(PluginBlockFluidClassic.Hooks.tryVaporizeHere(fluid, state, here, world, pos)) here = state; //try vaporize block here
            int level = state.getValue(BlockLiquid.LEVEL);

            final boolean isWater = state.getMaterial() == Material.WATER;
            final int flowCost = isWater || world.provider.doesWaterVaporize() ? 1 : 2;
            int tickRate = block.tickRate(world);

            //update level if non-source
            if(level > 0) {
                //check neighbor fluid blocks
                int currentMinLevel = -100;
                int adjacentSourceBlocks = 0;
                for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                    int neighborLevel = getDepth(fluid, world, pos.offset(facing), facing.getOpposite());
                    if(neighborLevel >= 0) {
                        if(neighborLevel == 0) ++adjacentSourceBlocks;
                        if(neighborLevel >= 8) {
                            neighborLevel = 0;
                        }

                        currentMinLevel = currentMinLevel >= 0 && neighborLevel >= currentMinLevel ? currentMinLevel : neighborLevel;
                    }
                }

                int newLevel = currentMinLevel + flowCost;
                if(newLevel >= 8 || currentMinLevel < 0) newLevel = -1;

                //check for fluid above this
                final int upLevel = getDepth(fluid, world, pos.up(), EnumFacing.DOWN);
                if(upLevel >= 0) {
                    if(upLevel >= 8) newLevel = upLevel;
                    else newLevel = upLevel + 8;
                }

                //create source block
                if(adjacentSourceBlocks >= 2 && ForgeEventFactory.canCreateFluidSource(world, pos, state, isWater)) {
                    final IBlockState down = world.getBlockState(pos.down());
                    if(down.getMaterial().isSolid()) newLevel = 0;
                    else if(getDepth(fluid, world, pos.down(), down, EnumFacing.UP) == 0) newLevel = 0;
                }

                //randomize lava update ticks (vanilla feature)
                if(!isWater && newLevel < 8 && newLevel > level && rand.nextInt(4) != 0)
                    tickRate *= 4;

                //no change, place static block
                if(newLevel == level) block.placeStaticBlock(world, pos, state);

                //update level here
                else {
                    level = newLevel;
                    if(level < 0) world.setBlockToAir(pos);
                    else {
                        state = state.withProperty(BlockLiquid.LEVEL, level);
                        world.setBlockState(pos, state, 2);
                        world.scheduleUpdate(pos, block, tickRate);
                        world.notifyNeighborsOfStateChange(pos, block, false);
                    }
                }
            }

            else {
                //place static block if source
                if(here == state) block.placeStaticBlock(world, pos, state);
                //try flowing to nearby fluidloggable blocks
                PluginBlockFluidClassic.Hooks.tryFlowIntoFluidloggable((IFluidBlock)block, world, pos, EnumFacing.DOWN, state, here, 8, isWater, EnumFacing.HORIZONTALS);
            }

            //fluidlog vertical if possible
            if(FluidloggedAPIConfigHandler.verticalFluidloggedFluidSpread)
                PluginBlockFluidClassic.Hooks.tryFlowIntoFluidloggable((IFluidBlock)block, world, pos, EnumFacing.DOWN, state, here, 8, isWater, EnumFacing.DOWN);

            //flow down if possible
            final IBlockState down = world.getBlockState(pos.down());
            if(canFlowInto(fluid, block, world, pos, here, down, EnumFacing.DOWN)) {
                if(!isWater && down.getMaterial() == Material.WATER) {
                    world.setBlockState(pos.down(), ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos.down(), pos, Blocks.STONE.getDefaultState()));
                    block.triggerMixEffects(world, pos.down());
                    return;
                }

                flowInto(block, world, pos.down(), state, down, level >= 8 ? level : level + 8);
            }

            //flow from the sides if possible
            else if(level >= 0 && (level == 0 || block.isBlocked(world, pos.down(), down)) || !FluidloggedUtils.canFluidFlow(world, pos, here, EnumFacing.DOWN)) {
                final int newLevel = level >= 8 ? 1 : level + flowCost;
                if(newLevel >= 8) return;

                //find sides to flow into
                int lowestPriority = 1000;
                final List<Pair<EnumFacing, IBlockState>> positionsToFlow = new ArrayList<>();
                for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                    if(FluidloggedUtils.canFluidFlow(world, pos, here, facing)) {
                        final BlockPos neighborPos = pos.offset(facing);
                        final IBlockState neighbor = world.getBlockState(neighborPos);

                        if(!block.isBlocked(world, neighborPos, neighbor)) {
                            final FluidState fluidState = FluidloggedUtils.getFluidState(world, neighborPos, neighbor);
                            if(!FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), fluid) || fluidState.getLevel() > 0) {
                                final int priority = block.isBlocked(world, neighborPos.down(), world.getBlockState(neighborPos.down()))
                                    ? getSlopeDistance(block, world, neighborPos, 1, facing.getOpposite(), fluid) : 0;

                                if(priority < lowestPriority)
                                    positionsToFlow.clear();

                                if(priority <= lowestPriority) {
                                    positionsToFlow.add(Pair.of(facing, neighbor));
                                    lowestPriority = priority;
                                }
                            }
                        }
                    }
                }

                //flow into sides
                for(Pair<EnumFacing, IBlockState> entry : positionsToFlow)
                    if(canFlowInto(fluid, block, world, pos, here, entry.getValue(), entry.getKey()))
                        flowInto(block, world, pos.offset(entry.getKey()), state, entry.getValue(), newLevel);
            }
        }

        //helper
        public static int getDepth(@Nonnull Fluid fluid, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facing) {
            return getDepth(fluid, world, pos, world.getBlockState(pos), facing);
        }

        //helper
        public static int getDepth(@Nonnull Fluid fluid, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull EnumFacing facing) {
            if(!FluidloggedUtils.canFluidFlow(world, pos, here, facing)) return -1;

            final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos, here);
            return FluidloggedUtils.isCompatibleFluid(fluid, fluidState.getFluid()) ? fluidState.getLevel() : -1;
        }

        //helper
        public static int getSlopeDistance(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, int distance, @Nonnull EnumFacing sourceFacing, @Nonnull Fluid sourceFluid) {
            int lowestPriority = 1000;
            for(EnumFacing facing : EnumFacing.HORIZONTALS) {
                if(facing != sourceFacing) {
                    final BlockPos neighborPos = pos.offset(facing);
                    final IBlockState neighbor = world.getBlockState(neighborPos);

                    if(!block.isBlocked(world, neighborPos, neighbor)) {
                        final FluidState fluidState = FluidloggedUtils.getFluidState(world, neighborPos, neighbor);
                        if(!FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), sourceFluid) || fluidState.getLevel() > 0) {
                            if(!block.isBlocked(world, neighborPos.down(), world.getBlockState(neighborPos.down()))) return distance;
                            if(distance < block.getSlopeFindDistance(world)) {
                                final int priority = getSlopeDistance(block, world, neighborPos, distance + 1, facing.getOpposite(), sourceFluid);
                                if(priority < lowestPriority) lowestPriority = priority;
                            }
                        }
                    }
                }
            }

            return lowestPriority;
        }

        //helper
        public static boolean canFlowInto(@Nonnull Fluid fluid, @Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull IBlockState neighbor, @Nonnull EnumFacing facing) {
            if(!FluidloggedUtils.canFluidFlow(world, pos, here, facing)) return false;

            final BlockPos neighborPos = pos.offset(facing);
            if(block.isBlocked(world, neighborPos, neighbor)) return false;

            return !FluidloggedUtils.isCompatibleFluid(fluid, FluidloggedUtils.getFluidState(world, neighborPos, neighbor).getFluid());
        }

        //helper
        public static void flowInto(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos neighborPos, @Nonnull IBlockState state, @Nonnull IBlockState neighbor, int level) {
            if(neighbor.getMaterial() != Material.AIR) {
                if(state.getMaterial() == Material.LAVA) block.triggerMixEffects(world, neighborPos);
                else if(neighbor.getMaterial().isToolNotRequired())
                    neighbor.getBlock().dropBlockAsItem(world, neighborPos, neighbor, 0);
            }

            world.setBlockState(neighborPos, block.getDefaultState().withProperty(BlockLiquid.LEVEL, level));
        }
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import com.google.common.primitives.Ints;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidBase;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidClassic;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDynamicLiquid;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.IFluidBlock;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import static net.minecraft.util.EnumFacing.*;

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
         * //TODO rewrite to be based off vanilla logic instead copying the one for forge fluids
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
        /*
         * randomTick:
         * New code:
         * //fluidlogged lava does fire spread
         * @ASMGenerated
         * public void randomTick(World worldIn, BlockPos pos, IBlockState state, Random random)
         * {
         *     Hooks.randomLiquidTick(worldIn, pos, state, random);
         * }
         */
        addMethod(classNode, obfuscated ? "func_180645_a" : "randomTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V",
            "randomLiquidTick", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Ljava/util/Random;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );
        /*
         * neighborChanged:
         * New code:
         * //tick fluidlogged fluids when neighbors update
         * @ASMGenerated
         * public void neighborChanged(IBlockState state, World worldIn, BlockPos pos, Block blockIn, BlockPos fromPos)
         * {
         *     Hooks.neighborChanged(this, state, worldIn, pos);
         * }
         */
        addMethod(classNode, obfuscated ? "func_189540_a" : "neighborChanged", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Lnet/minecraft/util/math/BlockPos;)V",
            "neighborChanged", "(Lnet/minecraft/block/BlockLiquid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)V", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        /*
         * requiresUpdates:
         * New code:
         * //shouldn't require updates
         * @ASMGenerated
         * public boolean requiresUpdates()
         * {
         *     return false;
         * }
         */
        addMethod(classNode, obfuscated ? "func_149698_L" : "requiresUpdates", "()Z", null, null, generator ->
                generator.visitInsn(ICONST_0));

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void updateLiquidTick(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand) {
            if(world.isRemote) return;
            final int lavaDif = (state.getMaterial() == Material.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
            if(!world.isAreaLoaded(pos, 3 - lavaDif)) return; // Forge: avoid loading unloaded chunks

            final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(block);
            if(fluid == null) throw new IllegalStateException("A critical error has occurred, please issue a bug report!");

            //fluidlogged fluids will have a different state here than the state input
            final IBlockState here = world.getBlockState(pos);
            int quantaRemaining = 8 - state.getValue(BlockLiquid.LEVEL);

            // check adjacent block levels if non-source
            if(quantaRemaining < 8) {
                int adjacentSourceBlocks = 0;
                final int expQuanta;

                if(ForgeEventFactory.canCreateFluidSource(world, pos, state, state.getMaterial() == Material.WATER)) {
                    for(EnumFacing facing : HORIZONTALS) {
                        BlockPos offset = pos.offset(facing);

                        if(PluginBlockFluidClassic.Hooks.isSourceBlock((IFluidBlock)block, world, offset, world.getBlockState(offset), facing.getOpposite()))
                            adjacentSourceBlocks++;
                    }
                }

                // new source block
                final IBlockState vertical = world.getBlockState(pos.down());
                if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || PluginBlockFluidClassic.Hooks.isSourceBlock((IFluidBlock)block, world, pos.down(), vertical, UP)))
                    expQuanta = 8;

                // vertical flow into block
                else if(PluginBlockFluidBase.Hooks.hasVerticalFlow(world, pos, fluid, -1)) expQuanta = 8 - lavaDif;

                else {
                    int maxQuanta = -100;
                    for(EnumFacing side : HORIZONTALS) {
                        BlockPos offset = pos.offset(side);

                        if(FluidloggedUtils.canFluidFlow(world, pos, here, side) && FluidloggedUtils.canFluidFlow(world, offset, world.getBlockState(offset), side.getOpposite()))
                            maxQuanta = getLargerQuanta((IFluidBlock)block, world, offset, maxQuanta);
                    }

                    expQuanta = maxQuanta - lavaDif;
                }

                //place static block
                if(expQuanta == quantaRemaining) block.placeStaticBlock(world, pos, state);

                // decay calculation
                else {
                    quantaRemaining = expQuanta;

                    if(expQuanta <= 0 ) world.setBlockToAir(pos);
                    else {
                        world.setBlockState(pos, state.withProperty(BlockLiquid.LEVEL, 8 - expQuanta), Constants.BlockFlags.DEFAULT_AND_RERENDER);
                        world.scheduleUpdate(pos, block, block.tickRate(world));
                        world.notifyNeighborsRespectDebug(pos, block, false);
                    }
                }
            }

            //place static block
            else {
                PluginBlockFluidClassic.Hooks.tryFlowIntoFluidloggable((IFluidBlock)block, world, pos, DOWN, state, here, 8, state.getMaterial() == Material.WATER, HORIZONTALS);
                if(state == here) block.placeStaticBlock(world, pos, state);
            }

            // Fluidlog vertical if possible
            if(ConfigHandler.verticalFluidloggedFluidSpread)
                PluginBlockFluidClassic.Hooks.tryFlowIntoFluidloggable((IFluidBlock)block, world, pos, DOWN, state, here, 8, state.getMaterial() == Material.WATER, DOWN);

            // Flow vertically if possible
            if(FluidloggedUtils.canFluidFlow(world, pos, here, DOWN) && canDisplace(world, pos.down(), fluid)) {
                if(state.getMaterial() == Material.LAVA) {
                    final IBlockState down = world.getBlockState(pos.down());
                    if(down.getBlock().isReplaceable(world, pos.down()) && FluidloggedUtils.canFluidFlow(world, pos.down(), down, UP)) {
                        final FluidState fluidState = FluidloggedUtils.getFluidState(world, pos.down(), down);
                        if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) {
                            world.setBlockState(pos.down(), ForgeEventFactory.fireFluidPlaceBlockEvent(world, pos.down(), pos, Blocks.STONE.getDefaultState()));
                            block.triggerMixEffects(world, pos.down());
                            return;
                        }
                    }
                }

                flowIntoBlock(block, world, pos.down(), fluid, lavaDif);
                return;
            }

            // Flow outward if possible
            int flowMeta = 8 - quantaRemaining + lavaDif;
            if(flowMeta >= 8 || flowMeta <= 0) return;

            if(PluginBlockFluidClassic.Hooks.isSourceBlock((IFluidBlock)block, world, pos, here, null) || !isLiquidFlowingVertically(world, pos, fluid, -1)) {
                final boolean[] flowTo = getOptimalFlowDirections((IFluidBlock)block, world, pos, here);
                for(int i = 0; i < 4; i++)
                    if(flowTo[i] && FluidloggedUtils.canFluidFlow(world, pos, here, PluginBlockFluidClassic.Hooks.SIDES[i]))
                        flowIntoBlock(block, world, pos.offset(PluginBlockFluidClassic.Hooks.SIDES[i]), fluid, flowMeta);
            }
        }

        //helper
        public static boolean isLiquidFlowingVertically(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int densityDir) {
            final EnumFacing facingDir = (densityDir < 0) ? UP : DOWN;

            final IBlockState here = world.getBlockState(pos);
            if(!FluidloggedUtils.canFluidFlow(world, pos, here, facingDir.getOpposite())) return false;

            final IBlockState neighbor = world.getBlockState(pos.up(densityDir));
            return FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(world, pos.up(densityDir), neighbor).getFluid(), fluid)
                    || (FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidState(world, pos, here).getFluid(), fluid)
                    && canDisplace(world, pos.up(densityDir), fluid));
        }

        //helper
        public static boolean canDisplace(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid) {
            final IBlockState state = world.getBlockState(pos);
            if(state.getBlock().isAir(state, world, pos)) return true;
            //checks if this & a fluid here are the same
            else if(FluidloggedUtils.isCompatibleFluid(fluid, FluidloggedUtils.getFluidState(world, pos, state).getFluid())) return false;
            //predefined displacements
            else if(displacements().containsKey(state.getBlock())) return displacements.get(state.getBlock());

            final Material material = state.getMaterial();
            if(material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID) return false;

            final int density = BlockFluidBase.getDensity(world, pos);
            return density == Integer.MAX_VALUE || fluid.getDensity() > density;
        }

        //helper
        private static Map<Block, Boolean> displacements = null;
        public static Map<Block, Boolean> displacements() {
            if(displacements == null) displacements = PluginBlockFluidBase.Hooks.defaultDisplacements(new HashMap<>());
            return displacements;
        }

        //helper
        public static int getLargerQuanta(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare) {
            int quantaRemaining = PluginBlockFluidBase.Hooks.hasVerticalFlow(world, pos, block.getFluid(), -1) ? 8 : PluginBlockLiquid.Hooks.getQuantaValue(block, world, pos);
            if(quantaRemaining <= 0) return compare;

            return Math.max(quantaRemaining, compare);
        }

        //helper
        public static boolean[] getOptimalFlowDirections(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
            final int lavaDif = (block.getFluid() == FluidRegistry.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
            final int[] flowCost = { 1000, 1000, 1000, 1000 };

            for(int side = 0; side < 4; side++) {
                if(!FluidloggedUtils.canFluidFlow(world, pos, here, PluginBlockFluidClassic.Hooks.SIDES[side])) continue;

                BlockPos offset = pos.offset(PluginBlockFluidClassic.Hooks.SIDES[side]);
                IBlockState neighbor = world.getBlockState(offset);
                if(!canFlowInto(block, world, offset, neighbor) || PluginBlockFluidClassic.Hooks.isSourceBlock(block, world, offset, neighbor, null)) continue;

                if(canFlowInto(block, world, offset.down(), world.getBlockState(offset.down()))) flowCost[side] = 0;
                else flowCost[side] = calculateFlowCost(block, world, offset, lavaDif, side);
            }

            final int min = Ints.min(flowCost);
            final boolean[] isOptimalFlowDirection = new boolean[4];
            for(int side = 0; side < 4; side++) isOptimalFlowDirection[side] = (flowCost[side] == min);

            return isOptimalFlowDirection;
        }

        //helper
        public static int calculateFlowCost(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, int recurseDepth, int side) {
            final int lavaDif = (block.getFluid() == FluidRegistry.LAVA && !world.provider.doesWaterVaporize()) ? 2 : 1;
            int cost = 1000;

            for(int adjSide = 0; adjSide < 4; adjSide++) {
                if(PluginBlockFluidClassic.Hooks.SIDES[adjSide] == PluginBlockFluidClassic.Hooks.SIDES[side].getOpposite()) continue;
                BlockPos pos2 = pos.offset(PluginBlockFluidClassic.Hooks.SIDES[adjSide]);
                IBlockState neighbor = world.getBlockState(pos2);

                if(!canFlowInto(block, world, pos2, neighbor) || PluginBlockFluidClassic.Hooks.isSourceBlock(block, world, pos2, neighbor, null)) continue;
                else if(canFlowInto(block, world, pos2.down(), world.getBlockState(pos2.down()))) return recurseDepth;
                else if(recurseDepth > 2) continue;

                cost = Math.min(cost, calculateFlowCost(block, world, pos2, recurseDepth + lavaDif, adjSide));
            }

            return cost;
        }

        //helper
        public static boolean canFlowInto(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state) {
            return FluidloggedUtils.isCompatibleFluid(FluidloggedUtils.getFluidFromState(state), block.getFluid()) && state.getValue(BlockLiquid.LEVEL) > 0 || canDisplace(world, pos, block.getFluid());
        }

        //helper
        public static void flowIntoBlock(@Nonnull BlockDynamicLiquid block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int meta) {
            if(canDisplace(world, pos, fluid)) {
                final IBlockState state = world.getBlockState(pos);

                if(!state.getBlock().isAir(state, world, pos)) {
                    // Forge: Vanilla has a 'bug' where snowballs don't drop like every other block. So special case because ewww...
                    if(state.getBlock() != Blocks.SNOW_LAYER) state.getBlock().dropBlockAsItem(world, pos, state, 0);
                    if(fluid == FluidRegistry.LAVA) block.triggerMixEffects(world, pos);
                }

                world.setBlockState(pos, block.getDefaultState().withProperty(BlockLiquid.LEVEL, meta), Constants.BlockFlags.DEFAULT_AND_RERENDER);
            }
        }

        public static void neighborChanged(@Nonnull BlockLiquid block, @Nonnull IBlockState state, @Nonnull World worldIn, @Nonnull BlockPos pos) {
            if(!block.checkForMixing(worldIn, pos, state) && !FluidState.get(worldIn, pos).isEmpty())
                worldIn.scheduleUpdate(pos, block, block.tickRate(worldIn));
        }

        public static void randomLiquidTick(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random random) {
            if(state.getMaterial() == Material.LAVA) Blocks.LAVA.randomTick(worldIn, pos, state, random);
        }
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import java.util.LinkedList;
import java.util.Queue;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.getFluidState;
import static net.minecraft.util.EnumFacing.values;

/**
 * fixes drain interactions across all modded fluids & FluidStates
 * @author jbred
 *
 */
public final class PluginBlockSponge implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * absorb:
         * New code:
         * //absorb FluidStates
         * private boolean absorb(World worldIn, BlockPos pos)
         * {
         *     return Hooks.absorb(worldIn, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176312_d" : "absorb"),
            "absorb", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        private static boolean available = true;
        public static boolean absorb(@Nonnull World world, @Nonnull BlockPos origin) {
            //temporary fix for strange update bug
            if(available) {
                available = false;
                final Queue<Pair<BlockPos, Integer>> queue = new LinkedList<>();
                queue.add(Pair.of(origin, 0));
                int absorbed = 0;

                while(!queue.isEmpty()) {
                    final Pair<BlockPos, Integer> entry = queue.poll();
                    final BlockPos pos = entry.getKey();
                    final int distance = entry.getValue();

                    for(EnumFacing facing : values()) {
                        final BlockPos offset = pos.offset(facing);
                        final FluidState fluidState = getFluidState(world, offset);

                        if(!fluidState.isEmpty() && fluidState.getMaterial() == Material.WATER) {
                            //don't drain bad fluid blocks (looking at you BOP kelp)
                            if(fluidState.isValid()) {
                                fluidState.getFluidBlock().drain(world, offset, true);
                                if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                                absorbed++;
                            }
                            //drain bad fluid blocks
                            else if(world.setBlockToAir(pos)) {
                                world.playEvent(Constants.WorldEvents.BREAK_BLOCK_EFFECTS, offset, Block.getStateId(fluidState.getState()));
                                fluidState.getBlock().dropBlockAsItem(world, offset, fluidState.getState(), 0);
                                if(distance < 6) queue.add(Pair.of(offset, distance + 1));
                                absorbed++;
                            }
                        }
                    }
                }

                available = true;
                return absorbed > 0;
            }

            return false;
        }
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockSponge;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.apache.commons.lang3.tuple.Pair;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

/**
 * fixes drain interactions across all modded fluids & FluidStates
 * @author jbred
 *
 */
@Mixin(BlockSponge.class)
public abstract class MixinBlockSponge
{
    /**
     * @reason fixes fluidlogged interactions
     * @author jbred
     */
    @Overwrite
    private boolean absorb(@Nonnull World world, @Nonnull BlockPos origin) {
        final List<BlockPos> notifyList = new ArrayList<>();
        final Queue<Pair<BlockPos, Integer>> queue = new LinkedList<>();
        queue.add(Pair.of(origin, 0));

        while(!queue.isEmpty()) {
            Pair<BlockPos, Integer> entry = queue.poll();
            BlockPos pos = entry.getKey();
            int distance = entry.getValue();

            for(EnumFacing facing : EnumFacing.values()) {
                BlockPos offset = pos.offset(facing);
                IBlockState state = world.getBlockState(offset);

                if(FluidloggedUtils.getFluidOrReal(world, offset, state).getMaterial() == Material.WATER) {
                    boolean drained = FluidloggedUtils.getFluidFromState(state) != null
                            ? world.setBlockState(offset, Blocks.AIR.getDefaultState(), 2)
                            : FluidloggedUtils.setFluidState(world, offset, state, FluidState.EMPTY, false, 2);

                    if(drained) {
                        if(distance < 6) queue.add(Pair.of(offset, ++distance));
                        notifyList.add(offset);
                    }
                }
            }

            if(notifyList.size() > 64) break;
        }

        notifyList.forEach(pos -> world.notifyNeighborsOfStateChange(pos, Blocks.AIR, false));
        return notifyList.size() > 0;
    }
}

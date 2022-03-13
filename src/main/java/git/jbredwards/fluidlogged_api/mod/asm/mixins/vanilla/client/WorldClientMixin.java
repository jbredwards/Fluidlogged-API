package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.client;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.item.ItemStack;
import net.minecraft.profiler.Profiler;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.WorldProvider;
import net.minecraft.world.storage.ISaveHandler;
import net.minecraft.world.storage.WorldInfo;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * non-empty FluidStates call randomDisplayTick
 * @author jbred
 *
 */
@Mixin(WorldClient.class)
public abstract class WorldClientMixin extends World
{
    protected WorldClientMixin(@Nonnull ISaveHandler saveHandlerIn, @Nonnull WorldInfo info, @Nonnull WorldProvider providerIn, @Nonnull Profiler profilerIn, boolean client) {
        super(saveHandlerIn, info, providerIn, profilerIn, client);
    }

    /**
     * @reason allow for fluidlogged block random ticks & remove hardcoded barrier behavior (it gets moved to BlockBarrier directly)
     * @author jbred
     */
    @Overwrite
    public void showBarrierParticles(int xIn, int yIn, int zIn, int offset, @Nonnull Random random, boolean holdingBarrier, @Nonnull BlockPos.MutableBlockPos pos) {
        final int x = xIn + rand.nextInt(offset) - rand.nextInt(offset);
        final int y = yIn + rand.nextInt(offset) - rand.nextInt(offset);
        final int z = zIn + rand.nextInt(offset) - rand.nextInt(offset);

        final IBlockState here = getBlockState(pos.setPos(x, y, z));
        here.getBlock().randomDisplayTick(here, this, pos, random);

        final FluidState fluidState = FluidState.get(pos);
        if(!fluidState.isEmpty()) fluidState.getBlock().randomDisplayTick(fluidState.getState(), this, pos, random);
    }

    //boost performance a tad, cause why not
    @Nonnull
    @Redirect(method = "doVoidFogParticles", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/entity/EntityPlayerSP;getHeldItemMainhand()Lnet/minecraft/item/ItemStack;"))
    private ItemStack doVoidFogParticles(@Nonnull EntityPlayerSP player) { return ItemStack.EMPTY; }
}

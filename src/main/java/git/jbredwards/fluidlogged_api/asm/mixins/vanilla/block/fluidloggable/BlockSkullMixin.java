package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.common.storage.FluidState;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.BlockSkull;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;

/**
 * makes skulls fluidloggable by default
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockSkull.class)
public abstract class BlockSkullMixin extends BlockContainer implements IFluidloggable
{
    @Nonnull
    private static final Material SKULL = new Material(MapColor.AIR) {
        @Nonnull public Material setNoPushMobility() { return super.setNoPushMobility(); }
        @Nonnull public MapColor getMaterialMapColor() { return Material.CIRCUITS.getMaterialMapColor(); }
        public boolean isSolid()     { return false; }
        public boolean blocksLight() { return false; }
    }.setNoPushMobility();

    public BlockSkullMixin(@Nonnull Material materialIn, @Nonnull MapColor colorIn) { super(materialIn, colorIn); }
    public BlockSkullMixin(@Nonnull Material materialIn) { super(materialIn); }

    @Nonnull
    @Redirect(method = "<init>", at = @At(value = "FIELD", target = "Lnet/minecraft/block/material/Material;CIRCUITS:Lnet/minecraft/block/material/Material;"))
    private static Material material() { return SKULL; }

    @Redirect(method = "checkWitherSpawn", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z", ordinal = 1))
    private static boolean getFluidOrAir(World world, BlockPos pos, IBlockState air, int blockFlags) {
        final FluidState fluidState = FluidState.get(world, pos);
        return world.setBlockState(pos, fluidState.isEmpty() ? air : fluidState.getState(), blockFlags);
    }
}

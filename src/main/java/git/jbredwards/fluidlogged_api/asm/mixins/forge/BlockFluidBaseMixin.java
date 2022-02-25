package git.jbredwards.fluidlogged_api.asm.mixins.forge;

import net.minecraft.block.Block;
import net.minecraft.block.BlockStairs;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockFaceShape;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Desc;
import org.spongepowered.asm.mixin.injection.ModifyArg;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.Map;

import static git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils.*;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mixin(BlockFluidBase.class)
public abstract class BlockFluidBaseMixin extends Block
{
    @Shadow(remap = false)
    protected int quantaPerBlock, densityDir;

    @Final
    @Shadow(remap = false)
    protected Fluid definedFluid;

    public BlockFluidBaseMixin(@Nonnull Material materialIn) { super(materialIn); }

    /*@Nonnull
    @Overwrite
    public Vec3d getFlowVector(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {

    }*/

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite(remap = false)
    final boolean hasVerticalFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
        final EnumFacing facing = (densityDir < 0) ? EnumFacing.UP : EnumFacing.DOWN;
        if(!canFluidFlow(world, pos, world.getBlockState(pos), facing)) return false;

        final BlockPos offset = pos.down(densityDir);
        final IBlockState state = world.getBlockState(offset);

        return canFluidFlow(world, offset, state, facing.getOpposite())
                && isCompatibleFluid(getFluidState(world, offset, state).getFluid(), getFluid());
    }

    /**
     * @reason fixes fluidlogged interactions (console warns if no author comment present)
     * @author jbred
     */
    @Overwrite(remap = false)
    protected boolean causesDownwardCurrent(@Nonnull IBlockAccess worldIn, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
        final IBlockState state = worldIn.getBlockState(pos);
        if(canFluidFlow(worldIn, pos, state, side) && isCompatibleFluid(getFluidState(worldIn, pos, state).getFluid(), getFluid())) return false;
        else if(side == (densityDir < 0 ? EnumFacing.UP : EnumFacing.DOWN)) return true;
        else if(state.getMaterial() == Material.ICE) return false;
        else {
            final Block block = state.getBlock();
            boolean flag = isExceptBlockForAttachWithPiston(block) || block instanceof BlockStairs;
            return !flag && state.getBlockFaceShape(worldIn, pos, side) == BlockFaceShape.SOLID;
        }
    }

    /**
     * @reason expose definedFluid to significantly boost performance
     * @author jbred
     */
    @Nonnull
    @Overwrite(remap = false)
    public Fluid getFluid() { return definedFluid; }

    @Nonnull
    @Redirect(method = {
            "getDensity(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I",
            "getTemperature(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I",
            "getFlowDirection"
    }, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"), remap = false)
    private static IBlockState getBlockState(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Nonnull
    @Redirect(method = "getFogColor", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFogColor(@Nonnull World world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Nonnull
    @Redirect(method = "getStateAtViewpoint", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/IBlockAccess;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFogColor(@Nonnull IBlockAccess world, @Nonnull BlockPos upPos) {
        return getFluidOrReal(world, upPos);
    }

    @Shadow(remap = false)
    protected abstract int getFlowDecay(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);

    //prevents crash
    @Redirect(method = "<init>(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V", at = @At(value = "INVOKE", target = "Ljava/util/Map;putAll(Ljava/util/Map;)V", remap = false), remap = false)
    private void test(@Nonnull Map<Block, Boolean> displacements, @Nonnull Map<Block, Boolean> defaultDisplacements) {
        //builds the default map (moved into constructor to prevent crash)
        if(defaultDisplacements.isEmpty()) {
            defaultDisplacements.put(Blocks.OAK_DOOR,                       false);
            defaultDisplacements.put(Blocks.SPRUCE_DOOR,                    false);
            defaultDisplacements.put(Blocks.BIRCH_DOOR,                     false);
            defaultDisplacements.put(Blocks.JUNGLE_DOOR,                    false);
            defaultDisplacements.put(Blocks.ACACIA_DOOR,                    false);
            defaultDisplacements.put(Blocks.DARK_OAK_DOOR,                  false);
            defaultDisplacements.put(Blocks.TRAPDOOR,                       false);
            defaultDisplacements.put(Blocks.IRON_TRAPDOOR,                  false);
            defaultDisplacements.put(Blocks.OAK_FENCE,                      false);
            defaultDisplacements.put(Blocks.SPRUCE_FENCE,                   false);
            defaultDisplacements.put(Blocks.BIRCH_FENCE,                    false);
            defaultDisplacements.put(Blocks.JUNGLE_FENCE,                   false);
            defaultDisplacements.put(Blocks.DARK_OAK_FENCE,                 false);
            defaultDisplacements.put(Blocks.ACACIA_FENCE,                   false);
            defaultDisplacements.put(Blocks.NETHER_BRICK_FENCE,             false);
            defaultDisplacements.put(Blocks.OAK_FENCE_GATE,                 false);
            defaultDisplacements.put(Blocks.SPRUCE_FENCE_GATE,              false);
            defaultDisplacements.put(Blocks.BIRCH_FENCE_GATE,               false);
            defaultDisplacements.put(Blocks.JUNGLE_FENCE_GATE,              false);
            defaultDisplacements.put(Blocks.DARK_OAK_FENCE_GATE,            false);
            defaultDisplacements.put(Blocks.ACACIA_FENCE_GATE,              false);
            defaultDisplacements.put(Blocks.WOODEN_PRESSURE_PLATE,          false);
            defaultDisplacements.put(Blocks.STONE_PRESSURE_PLATE,           false);
            defaultDisplacements.put(Blocks.LIGHT_WEIGHTED_PRESSURE_PLATE,  false);
            defaultDisplacements.put(Blocks.HEAVY_WEIGHTED_PRESSURE_PLATE,  false);
            defaultDisplacements.put(Blocks.LADDER,                         false);
            defaultDisplacements.put(Blocks.IRON_BARS,                      false);
            defaultDisplacements.put(Blocks.GLASS_PANE,                     false);
            defaultDisplacements.put(Blocks.STAINED_GLASS_PANE,             false);
            defaultDisplacements.put(Blocks.PORTAL,                         false);
            defaultDisplacements.put(Blocks.END_PORTAL,                     false);
            defaultDisplacements.put(Blocks.COBBLESTONE_WALL,               false);
            defaultDisplacements.put(Blocks.BARRIER,                        false);
            defaultDisplacements.put(Blocks.STANDING_BANNER,                false);
            defaultDisplacements.put(Blocks.WALL_BANNER,                    false);
            defaultDisplacements.put(Blocks.CAKE,                           false);

            defaultDisplacements.put(Blocks.IRON_DOOR,     false);
            defaultDisplacements.put(Blocks.STANDING_SIGN, false);
            defaultDisplacements.put(Blocks.WALL_SIGN,     false);
            defaultDisplacements.put(Blocks.REEDS,         false);
        }

        displacements.putAll(defaultDisplacements);
    }
}

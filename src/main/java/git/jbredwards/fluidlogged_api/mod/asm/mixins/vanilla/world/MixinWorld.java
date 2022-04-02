package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.world.IChunkProvider;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.*;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
@Mixin(value = World.class, priority = 999)
public abstract class MixinWorld implements IBlockAccess, IChunkProvider
{
    @Nonnull
    private final World world = (World)(Object)this;

    /**
     * @reason ray traces now include fluidlogged fluid blocks
     * @author jbred
     */
    @SuppressWarnings("ConstantConditions")
    @Nullable
    @Overwrite
    public RayTraceResult rayTraceBlocks(@Nonnull Vec3d vec, @Nonnull Vec3d end, boolean stopOnLiquid, boolean ignoreBlockWithoutBoundingBox, boolean returnLastUncollidableBlock) {
        if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z) || Double.isNaN(end.x) || Double.isNaN(end.y) || Double.isNaN(end.z))
            return null;

        final int endX = MathHelper.floor(end.x);
        final int endY = MathHelper.floor(end.y);
        final int endZ = MathHelper.floor(end.z);
        int prevX = MathHelper.floor(vec.x);
        int prevY = MathHelper.floor(vec.y);
        int prevZ = MathHelper.floor(vec.z);

        BlockPos pos = new BlockPos(prevX, prevY, prevZ);
        @Nullable RayTraceResult result;

        //check FluidState
        if(stopOnLiquid) {
            final FluidState fluidState = FluidState.get(this, pos);
            if(!fluidState.isEmpty() && fluidState.getBlock().canCollideCheck(fluidState.getState(), true) && (!ignoreBlockWithoutBoundingBox || fluidState.getState().getCollisionBoundingBox(this, pos) != Block.NULL_AABB)) {
                result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                if(result != null) { return result; }
            }
        }

        IBlockState state = getBlockState(pos);
        if(state.getBlock().canCollideCheck(state, stopOnLiquid) && (!ignoreBlockWithoutBoundingBox || state.getCollisionBoundingBox(this, pos) != Block.NULL_AABB)) {
            result = state.collisionRayTrace(world, pos, vec, end);
            if(result != null) { return result; }
        }

        @Nullable RayTraceResult lastResult = null;
        for(int i = 200; i-- >= 0;) {
            if(Double.isNaN(vec.x) || Double.isNaN(vec.y) || Double.isNaN(vec.z))
                return null;

            if(prevX == endX && prevY == endY && prevZ == endZ)
                return returnLastUncollidableBlock ? lastResult : null;

            boolean flagX = true, flagY = true, flagZ = true;
            double x = 999, y = 999, z = 999;

            if(endX > prevX) x = prevX + 1;
            else if(endX < prevX) x = prevX;
            else flagX = false;

            if(endY > prevY) y = prevY + 1;
            else if(endY < prevY) y = prevY;
            else flagY = false;

            if(endZ > prevZ) z = prevZ + 1;
            else if(endZ < prevZ) z = prevZ;
            else flagZ = false;

            double coveredX = 999, coveredY = 999, coveredZ = 999;
            double distX = end.x - vec.x;
            double distY = end.y - vec.y;
            double distZ = end.z - vec.z;

            if(flagX) coveredX = (x - vec.x) / distX;
            if(flagY) coveredY = (y - vec.y) / distY;
            if(flagZ) coveredZ = (z - vec.z) / distZ;

            if(coveredX == -0) coveredX = -1.0E-4;
            if(coveredY == -0) coveredY = -1.0E-4;
            if(coveredZ == -0) coveredZ = -1.0E-4;

            //the general direction of the trace
            EnumFacing facing;

            if(coveredX < coveredY && coveredX < coveredZ) {
                facing = endX > prevX ? EnumFacing.WEST : EnumFacing.EAST;
                vec = new Vec3d(x, vec.y + distY * coveredX, vec.z + distZ * coveredX);
            }

            else if(coveredY < coveredZ) {
                facing = endY > prevY ? EnumFacing.DOWN : EnumFacing.UP;
                vec = new Vec3d(vec.x + distX * coveredY, y, vec.z + distZ * coveredY);
            }

            else {
                facing = endZ > prevZ ? EnumFacing.NORTH : EnumFacing.SOUTH;
                vec = new Vec3d(vec.x + distX * coveredZ, vec.y + distY * coveredZ, z);
            }

            prevX = MathHelper.floor(vec.x) - (facing == EnumFacing.EAST  ? 1 : 0);
            prevY = MathHelper.floor(vec.y) - (facing == EnumFacing.UP    ? 1 : 0);
            prevZ = MathHelper.floor(vec.z) - (facing == EnumFacing.SOUTH ? 1 : 0);

            pos = new BlockPos(prevX, prevY, prevZ);

            //check FluidState
            if(stopOnLiquid) {
                FluidState fluidState = FluidState.get(this, pos);

                if(!fluidState.isEmpty() && (!ignoreBlockWithoutBoundingBox || fluidState.getState().getCollisionBoundingBox(this, pos) != Block.NULL_AABB)) {
                    if(fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                        result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                        if(result != null) return result;
                    }

                    else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                }
            }

            state = getBlockState(pos);
            if(!ignoreBlockWithoutBoundingBox || state.getMaterial() == Material.PORTAL || state.getCollisionBoundingBox(this, pos) != Block.NULL_AABB) {
                if(state.getBlock().canCollideCheck(state, stopOnLiquid)) {
                    result = state.collisionRayTrace(world, pos, vec, end);
                    if(result != null) return result;
                }

                else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
            }
        }

        return returnLastUncollidableBlock ? lastResult : null;
    }

    @Nonnull
    @Redirect(method = {"getLight(Lnet/minecraft/util/math/BlockPos;Z)I", "getLightFromNeighborsFor"}, at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState getFluidOrReal(@Nonnull World self, @Nonnull BlockPos pos) { return FluidloggedUtils.getFluidOrReal(self, pos); }

    @Nullable
    @Override
    public Chunk getChunkFromBlockCoords(@Nonnull BlockPos pos) { return world.getChunk(pos); }

    /*@Nullable
    @Redirect(method = "isMaterialInBB", at = @At(value = "INVOKE", target = "Lnet/minecraft/block/Block;isAABBInsideMaterial(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;"))
    private static Boolean isMaterialInBB(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb, @Nonnull Material materialIn) {
        final @Nullable Boolean oldResult = block.isAABBInsideMaterial(world, pos, bb, materialIn);
        if(oldResult != null) return oldResult;

        final FluidState fluidState = FluidState.get(world, pos);
        if(fluidState.isEmpty()) return null;

        final @Nullable Boolean newResult = fluidState.getBlock().isAABBInsideMaterial(world, pos, bb, materialIn);
        if(newResult != null) return newResult;

        else return block.getDefaultState().getMaterial() == materialIn || fluidState.getMaterial() == materialIn;
    }*/
}

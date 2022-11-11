package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.*;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import org.apache.logging.log4j.core.util.Loader;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;
import static net.minecraft.util.EnumFacing.*;

/**
 * corrects a lot of FluidState related interactions
 * @author jbred
 *
 */
public final class PluginWorld implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        //setBlockState, line 401
        if(checkMethod(method, obfuscated ? "func_180501_a" : "setBlockState", "(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
            return 1;

        //setBlockToAir, line 456
        else if(checkMethod(method, obfuscated ? "func_175698_g" : "setBlockToAir", null))
            return 2;

        //destroyBlock, line 480
        else if(checkMethod(method, obfuscated ? "func_175655_b" : "destroyBlock", null))
            return 2;

        //neighborChanged, line 626
        else if(checkMethod(method, obfuscated ? "func_190524_a" : "neighborChanged", null))
            return 3;

        //handleMaterialAcceleration, line 2443
        else if(checkMethod(method, obfuscated ? "func_72918_a" : "handleMaterialAcceleration", null)) {
            return 4;
        }

        //isMaterialInBB, line 2494
        else if(checkMethod(method, obfuscated ? "func_72875_a" : "isMaterialInBB", null)) {
            return 5;
        }

        //changes some methods to use FluidloggedUtils#getFluidOrReal
        else if(checkMethod(method, obfuscated ? "func_72953_d" : "containsAnyLiquid", null)
        || checkMethod(method, obfuscated ? "func_147470_e" : "isFlammableWithin", null)
        || checkMethod(method, obfuscated ? "func_175696_F" : "isWater", null)
        || checkMethod(method, obfuscated ? "func_175705_a" : "getLightFromNeighborsFor", null)
        || checkMethod(method, obfuscated ? "func_175721_c" : "getLight", "(Lnet/minecraft/util/math/BlockPos;Z)I"))
            return 6;

        //isFlammableWithin, fix bug with lava level
        else if(method.name.equals(obfuscated ? "func_147470_e" : "isFlammableWithin")) return 7;

        //rayTraceBlocks, ray traces now include fluidlogged fluid blocks
        else if(checkMethod(method, obfuscated ? "func_147447_a" : "rayTraceBlocks", "(Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;"))
            return 8;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        return true;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/asm/impl/IChunkProvider");
        addMethod(classNode, "getChunkFromBlockCoords", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/world/World", obfuscated ? "func_175726_f" : "getChunk", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/world/chunk/Chunk;", false);
        });
        //rayTraceBlocks, ray traces now include fluidlogged fluid blocks
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_147447_a" : "rayTraceBlocks", "(Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;"),
            "rayTraceBlocks", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/Vec3d;Lnet/minecraft/util/math/Vec3d;ZZZ)Lnet/minecraft/util/math/RayTraceResult;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ILOAD, 5);
            }
        );

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isFlammableWithin(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
            if(getFluidFromBlock(block) == FluidRegistry.LAVA) return Boolean.TRUE.equals(block.isAABBInsideLiquid(world, pos, bb));
            final FluidState fluidState = FluidState.get(world, pos); //handle possible lava FluidState
            return fluidState.getFluid() == FluidRegistry.LAVA && Boolean.TRUE.equals(fluidState.getBlock().isAABBInsideLiquid(world, pos, bb));
        }

        public static boolean isMaterialInBB(@Nonnull World world, @Nonnull AxisAlignedBB bb, @Nonnull Material materialIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
            for(int x = minX; x < maxX; ++x) {
                for(int y = minY; y < maxY; ++y) {
                    for(int z = minZ; z < maxZ; ++z) {
                        BlockPos pos = new BlockPos(x, y, z);
                        FluidState fluidState = FluidState.get(world, pos);

                        if(!fluidState.isEmpty()) {
                            @Nullable Boolean result = fluidState.getBlock().isAABBInsideMaterial(world, pos, bb, materialIn);
                            if(result != null) {
                                if(!result) continue;
                                return true;
                            }
                            else if(fluidState.getMaterial() == materialIn)
                                return true;
                        }
                    }
                }
            }

            return false;
        }

        public static boolean handleMaterialAcceleration(@Nonnull BlockPos.PooledMutableBlockPos pos, @Nonnull World world, @Nonnull Material material, @Nonnull Entity entity, @Nonnull Vec3d vec3dIn, boolean flagIn, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
            boolean flag = flagIn;

            for(int x = minX; x < maxX; ++x) {
                for(int y = minY; y < maxY; ++y) {
                    for(int z = minZ; z < maxZ; ++z) {
                        FluidState fluidState = FluidState.get(world, pos.setPos(x, y, z));
                        if(!fluidState.isEmpty()) {
                            Block block = fluidState.getBlock();

                            @Nullable Boolean result = block.isEntityInsideMaterial(world, pos, fluidState.getState(), entity, maxY, material, false);
                            if(Boolean.TRUE.equals(result)) {
                                Vec3d vec3d = block.modifyAcceleration(world, pos, entity, vec3dIn);
                                vec3dIn.x = vec3d.x;
                                vec3dIn.y = vec3d.y;
                                vec3dIn.z = vec3d.z;
                                flag = true;
                            }

                            else if(!Boolean.FALSE.equals(result) && fluidState.getMaterial() == material) {
                                //check for fluid height
                                double fluidHeight = y + 1 - (1 / 9.0f);
                                if(maxY >= fluidHeight) {
                                    Vec3d vec3d = block.modifyAcceleration(world, pos, entity, vec3dIn);
                                    vec3dIn.x = vec3d.x;
                                    vec3dIn.y = vec3d.y;
                                    vec3dIn.z = vec3d.z;
                                    flag = true;
                                }
                            }
                        }
                    }
                }
            }

            pos.release();
            return flag;
        }

        public static void neighborChanged(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Block blockIn, @Nonnull BlockPos fromPos, @Nonnull IBlockState here) {
            final FluidState fluidState = FluidState.get(world, pos);
            if(!fluidState.isEmpty()) {
                fluidState.getState().neighborChanged(world, pos, blockIn, fromPos);
                //update neighboring fluids in case the block here uses getActualState for canFluidFlow
                for(EnumFacing facing : VALUES) {
                    if(!canFluidFlow(world, pos, here, facing)) {
                        getFluidState(world, pos.offset(facing)).getState()
                                .neighborChanged(world, pos.offset(facing), here.getBlock(), pos);
                    }
                }
            }
        }

        @Nullable
        public static IBlockState setBlockState(@Nonnull Chunk chunk, @Nonnull BlockPos pos, @Nonnull IBlockState newState, @Nonnull IBlockState oldState, @Nonnull World world, int flags) {
            final @Nullable IBlockState chunkState = chunk.setBlockState(pos, newState);
            if(chunkState != null) {
                final FluidState fluidState = FluidState.getFromProvider(chunk, pos);
                //this mod adds a special flag (x | 32, example: Constants.BlockFlags.DEFAULT | 32) that removes any FluidState here
                if((flags & 32) != 0) {
                    if(!fluidState.isEmpty())
                        setFluidState(world, pos, newState, FluidState.EMPTY, false, false, flags);
                }

                //without the flag preserves FluidState / sets FluidState using oldState if it's a full fluid & if newState is fluidloggable
                else {
                    //remove FluidState here, new state isn't fluidloggable
                    if(!fluidState.isEmpty()) {
                        if(!isStateFluidloggable(newState, world, pos, fluidState.getFluid()))
                            setFluidState(world, pos, newState, FluidState.EMPTY, false, false, flags);
                            //ensure fluids are updated when the block here changes
                        else if(world.isAreaLoaded(pos, 1)) notifyFluids(world, pos, fluidState, true);
                    }
                    //save oldState as FluidState
                    else {
                        final @Nullable Fluid fluid = getFluidFromState(oldState);
                        if(fluid != null && isFluidloggableFluid(oldState, world, pos) && isStateFluidloggable(newState, world, pos, fluid))
                            setFluidState(world, pos, newState, FluidState.of(fluid), false, false, flags);
                    }
                }
            }

            return chunkState;
        }

        @SuppressWarnings("ConstantConditions")
        @Nullable
        public static RayTraceResult rayTraceBlocks(@Nonnull World world, @Nonnull Vec3d vec, @Nonnull Vec3d end, boolean stopOnLiquid, boolean ignoreBlockWithoutBoundingBox, boolean returnLastUncollidableBlock) {
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
                final FluidState fluidState = FluidState.get(world, pos);
                if(!fluidState.isEmpty() && fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                    result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                    if(result != null) { return result; }
                }
            }

            IBlockState state = world.getBlockState(pos);
            if(state.getBlock().canCollideCheck(state, stopOnLiquid) && (stopOnLiquid && getFluidFromState(state) != null || !ignoreBlockWithoutBoundingBox || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB)) {
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
                    facing = endX > prevX ? WEST : EAST;
                    vec = new Vec3d(x, vec.y + distY * coveredX, vec.z + distZ * coveredX);
                }

                else if(coveredY < coveredZ) {
                    facing = endY > prevY ? DOWN : UP;
                    vec = new Vec3d(vec.x + distX * coveredY, y, vec.z + distZ * coveredY);
                }

                else {
                    facing = endZ > prevZ ? NORTH : SOUTH;
                    vec = new Vec3d(vec.x + distX * coveredZ, vec.y + distY * coveredZ, z);
                }

                prevX = MathHelper.floor(vec.x) - (facing == EAST  ? 1 : 0);
                prevY = MathHelper.floor(vec.y) - (facing == UP    ? 1 : 0);
                prevZ = MathHelper.floor(vec.z) - (facing == SOUTH ? 1 : 0);

                pos = new BlockPos(prevX, prevY, prevZ);

                //check FluidState
                if(stopOnLiquid) {
                    FluidState fluidState = FluidState.get(world, pos);
                    if(!fluidState.isEmpty()) {
                        if(fluidState.getBlock().canCollideCheck(fluidState.getState(), true)) {
                            result = fluidState.getState().collisionRayTrace(world, pos, vec, end);
                            if(result != null) return result;
                        }

                        else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                    }
                }

                state = world.getBlockState(pos);
                if(!ignoreBlockWithoutBoundingBox || state.getMaterial() == Material.PORTAL || stopOnLiquid && getFluidFromState(state) != null || state.getCollisionBoundingBox(world, pos) != Block.NULL_AABB) {
                    if(state.getBlock().canCollideCheck(state, stopOnLiquid)) {
                        result = state.collisionRayTrace(world, pos, vec, end);
                        if(result != null) return result;
                    }

                    else lastResult = new RayTraceResult(RayTraceResult.Type.MISS, vec, facing, pos);
                }
            }

            return returnLastUncollidableBlock ? lastResult : null;
        }
    }
}

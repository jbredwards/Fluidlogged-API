/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.common.fluid.handler.FluidCollisionHandler;
import git.jbredwards.fluidlogged_api.mod.common.fluid.util.FluidCache;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.crash.CrashReport;
import net.minecraft.crash.CrashReportCategory;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.util.ReportedException;
import net.minecraft.util.math.*;
import net.minecraft.world.ChunkCache;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public final class PluginEntity implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_180799_ab" : "isInLava")) return 1;
        else if(method.name.equals(obfuscated ? "func_70072_I" : "handleWaterMovement")) return 2;
        else if(method.name.equals(obfuscated ? "func_71061_d_" : "doWaterSplashEffect")) return 3;
        else if(method.name.equals(obfuscated ? "func_70055_a" : "isInsideOfMaterial")) return 4;
        // else if(method.name.equals(obfuscated ? "func_174809_b" : "isLiquidPresentInAABB")) return 5;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * isInLava: (changes are around line 1466)
         * Old code:
         * return this.world.isMaterialInBB(this.getEntityBoundingBox().grow(-0.10000000149011612D, -0.4000000059604645D, -0.10000000149011612D), Material.LAVA);
         *
         * New code:
         * //don't change the AABB prior to checking lava collision
         * return this.world.isMaterialInBB(this.getEntityBoundingBox(), Material.LAVA);
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_72314_b" : "grow")) {
            removeFrom(instructions, insn, -3);
            return true;
        }
        /*
         * handleWaterMovement: (changes are around 1357)
         * Old code:
         * else if (this.world.handleMaterialAcceleration(this.getEntityBoundingBox().grow(0.0D, -0.4000000059604645D, 0.0D).shrink(0.001D), Material.WATER, this))
         * {
         *     ...
         * }
         *
         * New code:
         * //don't change the AABB prior to checking water collision unless squid
         * else if (this.world.handleMaterialAcceleration(this.getEntityBoundingBox().grow(0.0D, Hooks.fixSquidWaterCollision(-0.4000000059604645D, this), 0.0D).shrink(0.001D), Material.WATER, this))
         * {
         *     ...
         * }
         */
        else if(index == 2 && checkMethod(getNext(insn, 2), obfuscated ? "func_72314_b" : "grow")) {
            instructions.insert(insn, genMethodNode("fixSquidWaterCollision", "(DLnet/minecraft/entity/Entity;)D"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            return true;
        }
        //doWaterSplashEffect
        else if(index == 3) {
            /*
             * doWaterSplashEffect: (changes are around line 1392)
             * Old code:
             * float f2 = (float)MathHelper.floor(this.getEntityBoundingBox().minY);
             *
             * New code:
             * //fix height where the splash particles spawn
             * float f2 = Hooks.fixWaterSplashEffect(this);
             */
            if(checkMethod(insn.getPrevious(), obfuscated ? "func_76128_c" : "floor")) {
                instructions.insert(insn, genMethodNode("fixWaterSplashEffect", "(Lnet/minecraft/entity/Entity;)F"));
                removeFrom(instructions, insn, -3);
            }
            /*
             * doWaterSplashEffect: (changes are around lines 1398 & 1405)
             * Old code:
             * this.world.spawnParticle(EnumParticleTypes.WATER_BUBBLE, this.posX + (double)f3, (double)(f2 + 1.0F), this.posZ + (double)f4, this.motionX, this.motionY - (double)(this.rand.nextFloat() * 0.2F), this.motionZ);
             *
             * New code:
             * //remove y offset & particle motion
             * this.world.spawnParticle(EnumParticleTypes.WATER_BUBBLE, this.posX + (double)f3, (double)(f2), this.posZ + (double)f4, 0, 0, 0);
             */
            else if(insn.getOpcode() == FADD && insn.getPrevious().getOpcode() == FCONST_1) removeFrom(instructions, insn, -1);
            else if(checkField(insn, "WATER_BUBBLE")) {
                removeFrom(instructions, getNext(insn, 15), 12);
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
                instructions.insert(getNext(insn, 14), new InsnNode(DCONST_0));
            }
        }
        /*
         * isInsideOfMaterial: (changes are around line 1450)
         * Old code:
         * Boolean result = iblockstate.getBlock().isEntityInsideMaterial(this.world, blockpos, iblockstate, this, d0, materialIn, true);
         *
         * New code:
         * //add FluidState functionality
         * Boolean result = Hooks.isEntityInsideFluidState(iblockstate.getBlock(), this.world, blockpos, iblockstate, this, d0, materialIn, true);
         */
        else if(index == 4 && checkMethod(insn, "isEntityInsideMaterial")) {
            instructions.insert(insn, genMethodNode("isEntityInsideFluidState", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * isLiquidPresentInAABB: (changes are around line 678)
         * Old code:
         * return this.world.getCollisionBoxes(this, bb).isEmpty() && !this.world.containsAnyLiquid(bb);
         *
         * New code
         * //fix issue#151
         * return this.world.getCollisionBoxes(this, bb).isEmpty() && !Hooks.blocksContainAnyLiquid(this.world, bb);
         */
        else if(index == 5 && checkMethod(insn, obfuscated ? "func_72953_d" : "containsAnyLiquid")) {
            instructions.insert(insn, genMethodNode("blocksContainAnyLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/AxisAlignedBB;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/entity/PluginEntity$Accessor");
        addMethod(classNode, "onInsideBlock_Public", "(Lnet/minecraft/block/state/IBlockState;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/entity/Entity", obfuscated ? "func_191955_a" : "onInsideBlock", "(Lnet/minecraft/block/state/IBlockState;)V", false);
        });

        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IWaterHeight");
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "waterHeight", "Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;", null, null));
        addMethod(classNode, "getBox", "()Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/entity/Entity", "waterHeight", "Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;");
        });
        addMethod(classNode, "setBox", "(Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/entity/Entity", "waterHeight", "Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;");
        });

        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_145775_I" : "doBlockCollisions"), "doBlockCollisions", "(Lnet/minecraft/entity/Entity;)V", generator -> generator.visitVarInsn(ALOAD, 0));
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean blocksContainAnyLiquid(@Nonnull final World world, @Nonnull final AxisAlignedBB bb) {
            @Nonnull final FluidCache cache = new FluidCache(world, bb);

            for(int x = cache.minX; x < cache.maxX; x++) {
                for(int y = cache.minY; y < cache.maxY; y++) {
                    for(int z = cache.minZ; z < cache.maxZ; z++) {
                        // block
                        {
                            @Nonnull final IBlockState here = cache.getBlockState(cache.mutablePos.setPos(x, y, z));
                            if(FluidloggedUtils.isFluid(here)) return true;
                            @Nullable final Boolean result = here.getBlock().isAABBInsideLiquid(world, cache.mutablePos.setPos(x, y, z), bb);
                            if(Boolean.TRUE.equals(result)) return true;
                        }
                        // fluid
                        {
                            @Nonnull final FluidState here = cache.getFluidState(cache.mutablePos.setPos(x, y, z));
                            if(!here.isEmpty()) return true;
                            @Nullable final Boolean result = here.getBlock().isAABBInsideLiquid(world, cache.mutablePos.setPos(x, y, z), bb);
                            if(Boolean.TRUE.equals(result)) return true;
                        }
                    }
                }
            }

            return false;
        }

        public static void doBlockCollisions(@Nonnull final Entity entity) {
            @Nonnull final BlockPos.PooledMutableBlockPos start = BlockPos.PooledMutableBlockPos.retain(entity.getEntityBoundingBox().minX + 0.001, entity.getEntityBoundingBox().minY + 0.001, entity.getEntityBoundingBox().minZ + 0.001);
            @Nonnull final BlockPos.PooledMutableBlockPos end = BlockPos.PooledMutableBlockPos.retain(entity.getEntityBoundingBox().maxX - 0.001, entity.getEntityBoundingBox().maxY - 0.001, entity.getEntityBoundingBox().maxZ - 0.001);

            if(entity.world.isAreaLoaded(start, end)) {
                @Nonnull final ChunkCache access = new ChunkCache(entity.world, start, end, 0);
                BlockPos.getAllInBoxMutable(start, end).forEach(pos -> {
                    // for IBlockState
                    @Nonnull final IBlockState state = access.getBlockState(pos);
                    if(!Boolean.FALSE.equals(state.getBlock().isAABBInsideLiquid(entity.world, pos, entity.getEntityBoundingBox()))) {
                        try {
                            state.getBlock().onEntityCollision(entity.world, pos, state, entity);
                            ((PluginEntity.Accessor)entity).onInsideBlock_Public(state);
                        }
                        catch(@Nonnull final Throwable t) {
                            @Nonnull final CrashReport report = CrashReport.makeCrashReport(t, "Colliding entity with block");
                            CrashReportCategory.addBlockInfo(report.makeCategory("Block being collided with"), pos, state);
                            throw new ReportedException(report);
                        }
                    }

                    // for FluidState
                    @Nonnull final IBlockState fluidState = FluidState.get(access, pos).getState();
                    if(!Boolean.FALSE.equals(fluidState.getBlock().isAABBInsideLiquid(entity.world, pos, entity.getEntityBoundingBox()))) {
                        try {
                            fluidState.getBlock().onEntityCollision(entity.world, pos, fluidState, entity);
                            ((PluginEntity.Accessor)entity).onInsideBlock_Public(fluidState);
                        }
                        catch(@Nonnull final Throwable t) {
                            @Nonnull final CrashReport report = CrashReport.makeCrashReport(t, "Colliding entity with fluid");
                            CrashReportCategory.addBlockInfo(report.makeCategory("Fluid being collided with"), pos, fluidState);
                            throw new ReportedException(report);
                        }
                    }
                });
            }

            start.release();
            end.release();
        }

        public static double fixSquidWaterCollision(double factor, @Nonnull Entity entity) {
            return entity.isCreatureType(EnumCreatureType.WATER_CREATURE, false) ? factor : 0;
        }

        public static float fixWaterSplashEffect(@Nonnull Entity entity) {
            final @Nullable RayTraceResult result = entity.world.rayTraceBlocks(
                    new Vec3d(entity.posX - entity.motionX, entity.posY - entity.motionY, entity.posZ - entity.motionZ),
                    new Vec3d(entity.posX, entity.posY, entity.posZ),
                    true, true, false);

            //use the exact point where the entity collided with water
            if(result != null) {
                final BlockPos pos = result.getBlockPos();
                final FluidState fluidState = FluidloggedUtils.getFluidState(entity.world, pos);
                if(!fluidState.isEmpty() && fluidState.isValid()) {
                    final float filled = FluidCollisionHandler.getFilledPercentage(fluidState, entity.world, pos);
                    return pos.getY() + (filled < 0 ? filled + 1.1f : filled - 0.1f);
                }
            }

            //estimate (should never pass)
            return (float)(entity.posY + entity.motionY * -0.7 - 0.1);
        }

        @Nullable
        public static Boolean isEntityInsideFluidState(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull Entity entity, double yToTest, @Nonnull Material materialIn, boolean testingHead) {
            @Nullable Boolean result = block.isEntityInsideMaterial(world, pos, here, entity, yToTest, materialIn, testingHead);
            if(result != null) return result;
            //check for FluidState if block here is not a fluid
            else if(FluidloggedUtils.getFluidFromBlock(block) == null) {
                final FluidState fluidState = FluidState.get(world, pos);
                if(!fluidState.isEmpty()) {
                    result = fluidState.getBlock().isEntityInsideMaterial(world, pos, fluidState.getState(), entity, yToTest, materialIn, testingHead);
                    if(result != null) return result;
                    else if(fluidState.getMaterial() == materialIn)
                        return ForgeHooks.isInsideOfMaterial(materialIn, entity, pos);
                }
            }

            return null;
        }
    }

    public interface Accessor
    {
        void onInsideBlock_Public(@Nonnull final IBlockState state);
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.passive.EntityWaterMob;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.getFluidFromBlock;
import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.getFluidState;

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
        else if(method.name.equals(obfuscated ? "func_145775_I" : "doBlockCollisions")) return 5;
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
         * doBlockCollisions: (changes are around line 1158)
         * Old code:
         * iblockstate.getBlock().onEntityCollision(this.world, blockpos$pooledmutableblockpos2, iblockstate, this);
         *
         * New code:
         * //fix fluid collisions & add FluidState functionality
         * Hooks.onEntityCollidedWithFluidState(iblockstate.getBlock(), this.world, blockpos$pooledmutableblockpos2, iblockstate, this);
         */
        else if(index == 5 && checkMethod(insn, obfuscated ? "func_180634_a" : "onEntityCollision")) {
            instructions.insert(insn, genMethodNode("onEntityCollidedWithFluidState", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;)V"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static double fixSquidWaterCollision(double factor, @Nonnull Entity entity) {
            return entity instanceof EntityWaterMob ? factor : 0;
        }

        public static float fixWaterSplashEffect(@Nonnull Entity entity) {
            final @Nullable RayTraceResult result = entity.world.rayTraceBlocks(
                    new Vec3d(entity.posX - entity.motionX, entity.posY - entity.motionY, entity.posZ - entity.motionZ),
                    new Vec3d(entity.posX, entity.posY, entity.posZ),
                    true, true, false);

            //use the exact point where the entity collided with water
            if(result != null) {
                final BlockPos pos = result.getBlockPos();
                final FluidState fluidState = getFluidState(entity.world, pos);
                if(!fluidState.isEmpty() && fluidState.isValid()) {
                    final float filled = fluidState.getFluidBlock().getFilledPercentage(entity.world, pos);
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
            else if(getFluidFromBlock(block) == null) {
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

        public static void onEntityCollidedWithFluidState(@Nonnull Block block, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nonnull Entity entityIn) {
            //check if the entity is inside the block before doing collisions
            if(!Boolean.FALSE.equals(block.isAABBInsideLiquid(worldIn, pos, entityIn.getEntityBoundingBox())))
                block.onEntityCollision(worldIn, pos, here, entityIn);

            //don't check for FluidState if block here is a fluid
            if(getFluidFromBlock(block) != null) return;
            final FluidState fluidState = FluidState.get(worldIn, pos);
            if(!fluidState.isEmpty() && !Boolean.FALSE.equals(fluidState.getBlock().isAABBInsideLiquid(worldIn, pos, entityIn.getEntityBoundingBox())))
                fluidState.getBlock().onEntityCollision(worldIn, pos, fluidState.getState(), entityIn);
        }
    }
}

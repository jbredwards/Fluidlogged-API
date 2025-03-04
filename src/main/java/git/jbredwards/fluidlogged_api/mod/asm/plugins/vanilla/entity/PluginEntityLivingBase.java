/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.entity;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IConfigFluidBox;
import git.jbredwards.fluidlogged_api.mod.asm.iface.IWaterHeight;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.MoverType;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.MathHelper;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fix issue#151
 * @author jbred
 *
 */
public final class PluginEntityLivingBase implements IASMPlugin
{
    int moveIndex;

    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_70636_d" : "onLivingUpdate")) return 1;
        else return method.name.equals(obfuscated ? "func_191986_a" : "travel") ? 2 : 0;
    }

    @Override
    public boolean transform(@Nonnull final InsnList instructions, @Nonnull final MethodNode method, @Nonnull final AbstractInsnNode insn, final boolean obfuscated, final int index) {
        if(index == 1) {
            /*
             * onLivingUpdate: (changes are around line 2598)
             * Old code:
             * if (this.isInWater())
             * {
             *     ...
             * }
             *
             * New code:
             * // don't swim in fluids that have a height of 0.4 blocks or less (vanilla behavior, and fixes issue#151)
             * if (Hooks.isInDeepWater(this))
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, obfuscated ? "func_70090_H" : "isInWater")) {
                instructions.insert(insn, genMethodNode("isInDeepWater", "(Lnet/minecraft/entity/Entity;)Z"));
                instructions.remove(insn);
            }
            /*
             * onLivingUpdate: (changes are around line 2606)
             * Old code:
             * else if (this.onGround && this.jumpTicks == 0)
             *
             * New code:
             * // allow jumping in fluids that have a height of 0.4 blocks or less (vanilla behavior, and fixes issue#151)
             * else if (Hooks.isInShallowWater(this) && this.jumpTicks == 0)
             * {
             *     ...
             * }
             */
            else if(checkField(insn, obfuscated ? "field_70122_E" : "onGround")) {
                instructions.insert(insn, genMethodNode("isInShallowWater", "(Lnet/minecraft/entity/Entity;)Z"));
                instructions.remove(insn);
                return true;
            }
        }
        if(index == 2) {
            /*
             * travel: (changes are around lines 2214 & 2254)
             * Old code:
             * this.move(MoverType.SELF, this.motionX, this.motionY, this.motionZ);
             *
             * New code:
             * // add ladder functionality while submerged
             * Hooks.moveWithLadder(MoverType.SELF, this.motionX, this.motionY, this.motionZ);
             */
            if(checkMethod(insn, obfuscated ? "func_70091_d" : "move") && moveIndex ++>= 1) {
                instructions.insert(insn, genMethodNode("moveWithLadder", "(Lnet/minecraft/entity/EntityLivingBase;Lnet/minecraft/entity/MoverType;DDD)V"));
                instructions.remove(insn);
            }
            /*
             * travel: (changes are around lines 2224 & 2264)
             * Old code:
             * if (this.collidedHorizontally && this.isOffsetPositionInLiquid(this.motionX, this.motionY + 0.6000000238418579D - this.posY + d4, this.motionZ))
             * {
             *     this.motionY = 0.30000001192092896D;
             * }
             *
             * New code:
             * //fix issue#151
             * if (this.collidedHorizontally && Hooks.isOffsetPositionInLiquid(this, this.motionX, this.motionY + 0.6000000238418579D - this.posY + d4, this.motionZ))
             * {
             *     this.motionY = 0.30000001192092896D;
             * }
             */
            else if(checkMethod(insn, obfuscated ? "func_70038_c" : "isOffsetPositionInLiquid")) {
                instructions.insert(insn, genMethodNode("isOffsetPositionInLiquid", "(Lnet/minecraft/entity/EntityLivingBase;DDD)Z"));
                instructions.remove(insn);
            }
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean isInDeepWater(@Nonnull final Entity entity) {
            if(!FluidloggedAPIConfig.ignoreLowFluidCollision || !entity.isPushedByWater() || !(entity instanceof EntityPlayer)) return entity.isInWater();
            else if(!entity.isInWater()) return false;
            else if(!entity.onGround) return true;

            @Nullable final IConfigFluidBox.HeightBox box = ((IWaterHeight)entity).getBox();
            return box != null && box.max - box.min > 0.4;
        }

        public static boolean isInShallowWater(@Nonnull final Entity entity) {
            if(!FluidloggedAPIConfig.ignoreLowFluidCollision || !entity.isPushedByWater() || !(entity instanceof EntityPlayer)) return entity.onGround;
            else if(entity.onGround) return true;
            else if(!entity.isInWater()) return false;

            @Nullable final IConfigFluidBox.HeightBox box = ((IWaterHeight)entity).getBox();
            return box != null && box.max - box.min <= 0.4;
        }

        public static boolean isOffsetPositionInLiquid(@Nonnull final EntityLivingBase entity, final double x, final double y, final double z) {
            return (entity.isJumping || !(entity instanceof EntityPlayer)) && !entity.isOnLadder() && entity.isOffsetPositionInLiquid(x, y, z);
        }

        public static void moveWithLadder(@Nonnull final EntityLivingBase entity, @Nonnull final MoverType type, final double x, final double y, final double z) {
            double moveX = x, moveY = y, moveZ = z;
            if(entity.isOnLadder()) {
                moveX = MathHelper.clamp(moveX, -0.15, 0.15);
                moveZ = MathHelper.clamp(moveZ, -0.15, 0.15);
                entity.fallDistance = 0;

                if(moveY < 0 && entity instanceof EntityPlayer && entity.isSneaking()) moveY = 0;
                else if(moveY < -0.15) moveY = -0.15;
            }

            entity.move(type, moveX, moveY, moveZ);
            if(entity.collidedHorizontally && entity.motionY < 0.16 && entity.isOnLadder()) entity.motionY = 0.16;
        }
    }
}

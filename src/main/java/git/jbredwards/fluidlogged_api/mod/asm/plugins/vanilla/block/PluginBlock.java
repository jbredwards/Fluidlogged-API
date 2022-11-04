package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLilyPad;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.IPlantable;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * fixes some lighting, canSustainPlant, and explosion related issues
 * @author jbred
 *
 */
public final class PluginBlock implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_176200_f" : "isReplaceable"))
            return 1;
        else if(checkMethod(method, "removedByPlayer", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/player/EntityPlayer;Z)Z"))
            return 2;
        else if(checkMethod(method, "getExplosionResistance", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/Explosion;)F"))
            return 3;
        else if(checkMethod(method, "canSustainPlant", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;Lnet/minecraftforge/common/IPlantable;)Z"))
            return 4;

        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * isReplaceable: (changes are around line 445)
         * Old code:
         * return worldIn.getBlockState(pos).getMaterial().isReplaceable();
         *
         * New code:
         * //use this material to improve performance
         * return this.material.isReplaceable();
         */
        if(index == 1 && checkMethod(insn, obfuscated ? "func_185904_a" : "getMaterial")) {
            //add blockMaterial#isReplaceable
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            removeFrom(instructions, insn, -3);
            return true;
        }
        /*
         * removedByPlayer: (changes are around line 1533)
         * Old code:
         * return world.setBlockState(pos, net.minecraft.init.Blocks.AIR.getDefaultState(), world.isRemote ? 11 : 3);
         *
         * New code:
         * //when a block is removed by a player, set the FluidState here (if it's empty air is set)
         * return world.setBlockState(pos, FluidState.get(world, pos).getState(), world.isRemote ? 11 : 3);
         */
        else if(index == 2 && checkField(insn, obfuscated ? "field_150350_a" : "AIR", "Lnet/minecraft/block/Block;")) {
            //parameters
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 3));
            //adds new code
            instructions.insertBefore(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidState", "get", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getState", "()Lnet/minecraft/block/state/IBlockState;", false));
            removeFrom(instructions, insn, 1);
            return true;
        }
        /*
         * getExplosionResistance: (changes are around line 1877)
         * Old code:
         * return getExplosionResistance(exploder);
         *
         * New code:
         * //use the FluidState explosion resistance value here if it's higher than this block's
         * return Hooks.getExplosionResistance(this, exploder, world, pos, explosion);
         */
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_149638_a" : "getExplosionResistance", "(Lnet/minecraft/entity/Entity;)F")) {
            final InsnList list = new InsnList();
            //parameters
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 4));
            //adds the new code
            list.add(genMethodNode("getExplosionResistance", "(Lnet/minecraft/block/Block;Lnet/minecraft/entity/Entity;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/Explosion;)F"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        else if(index == 4) {
            /*
             * canSustainPlant: (changes are around line 2060)
             * Old code:
             * if (plantable instanceof BlockBush && ((BlockBush)plantable).canSustainBush(state))
             * {
             *     ...
             * }
             *
             * New code:
             * //FluidStates can support lilypads if the block here is less than 1 block tall
             * if (plantable instanceof BlockBush && Hooks.canSustainLily(((BlockBush)plantable).canSustainBush(state), plantable, state, world, pos))
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, obfuscated ? "func_185514_i" : "canSustainBush")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 5));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, 3));
                list.add(genMethodNode("canSustainLily", "(ZLnet/minecraftforge/common/IPlantable;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z"));
                instructions.insert(insn, list);
            }
            /*
             * canSustainPlant: (changes are around line 2075)
             * Old code:
             * boolean hasWater = (world.getBlockState(pos.east()).getMaterial() == Material.WATER ||
             *                     world.getBlockState(pos.west()).getMaterial() == Material.WATER ||
             *                     world.getBlockState(pos.north()).getMaterial() == Material.WATER ||
             *                     world.getBlockState(pos.south()).getMaterial() == Material.WATER);
             *
             * New code:
             * //check for FluidStates
             * boolean hasWater = (FluidloggedUtils.getFluidOrReal(world, pos.east()).getMaterial() == Material.WATER ||
             *                     FluidloggedUtils.getFluidOrReal(world, pos.west()).getMaterial() == Material.WATER ||
             *                     FluidloggedUtils.getFluidOrReal(world, pos.north()).getMaterial() == Material.WATER ||
             *                     FluidloggedUtils.getFluidOrReal(world, pos.south()).getMaterial() == Material.WATER);
             */
            else if(checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
                instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
                instructions.remove(insn);
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        //used for overriding canFluidFlow behavior via the config
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;", null, null));
        /*
         * =========
         * Accessors
         * =========
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/block/PluginBlock$Accessor");
        /*
         * Accessor:
         * New code:
         * //getter for canFluidFlow
         * @ASMGenerated
         * public ConfigHandler.ICanFluidFlowHandler getCanFluidFlow()
         * {
         *     return this.canFluidFlow;
         * }
         */
        addMethod(classNode, "getCanFluidFlow", "()Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;");
        });
        /*
         * Accessor:
         * New code:
         * //setter for canFluidFlow
         * @ASMGenerated
         * public void setCanFluidFlow(ConfigHandler.ICanFluidFlowHandler canFluidFlow)
         * {
         *     this.canFluidFlow = canFluidFlow;
         * }
         */
        addMethod(classNode, "setCanFluidFlow", "(Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/Block", "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/common/config/ConfigHandler$ICanFluidFlowHandler;");
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean canSustainLily(boolean canSustainBush, @Nonnull IPlantable plantable, @Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            if(canSustainBush) return true;
            else if(!(plantable instanceof BlockLilyPad)) return false;
            else return state.getBoundingBox(world, pos).maxY < 1
                        && FluidState.get(world, pos).getMaterial() == Material.WATER;
        }

        @SuppressWarnings("ConstantConditions")
        public static float getExplosionResistance(@Nonnull Block block, @Nullable Entity exploder, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Explosion explosion) {
            if(FluidloggedUtils.isFluid(block)) return block.getExplosionResistance(exploder);
            //return the greater of the two possible resistance values here
            final FluidState fluidState = FluidState.get(world, pos);
            if(fluidState.isEmpty()) return block.getExplosionResistance(exploder);

            return Math.max(fluidState.getBlock().getExplosionResistance(world, pos, exploder, explosion),
                    block.getExplosionResistance(exploder));
        }
    }

    public interface Accessor
    {
        @Nullable
        ConfigHandler.ICanFluidFlowHandler getCanFluidFlow();
        void setCanFluidFlow(@Nullable ConfigHandler.ICanFluidFlowHandler canFluidFlow);
    }
}

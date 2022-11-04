package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.forge.PluginBlockFluidBase;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.IFluidBlock;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;
import static net.minecraft.util.EnumFacing.DOWN;
import static net.minecraft.util.EnumFacing.HORIZONTALS;

/**
 * makes liquids fluidloggable
 * @author jbred
 *
 */
public final class PluginBlockLiquid implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals(obfuscated ? "func_149645_b" : "getRenderType")) return 1;
        else if(method.name.equals(obfuscated ? "func_176365_e" : "checkForMixing")) return 2;
        return method.name.equals("getFogColor") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getRenderType: (changes are around line 168)
         * Old code:
         * return EnumBlockRenderType.LIQUID;
         *
         * New code:
         * //change render type, so it can be handled by forge's fluid rendering system
         * return EnumBlockRenderType.MODEL;
         */
        if(index == 1 && checkField(insn, "LIQUID")) ((FieldInsnNode)insn).name = "MODEL";
        //stone & obsidian only form while directly connected to lava/water
        else if(index == 2) {

        }
        //fix fog color to work with new fluid collision
        else if(index == 3) {
            /*
             * getFogColor: (changes are around line 549)
             * Old code:
             * if (state.getMaterial().isLiquid())
             * {
             *     ....
             * }
             *
             * New code:
             * //move all fluid checks to this method.
             * //if this returns true, return oldColor, else return super.getFogColor(...)
             * if (Hooks.getLiquidFogColor(state, world, pos, viewport))
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, obfuscated ? "func_76224_d" : "isLiquid")) {
                instructions.insert(insn, genMethodNode("getLiquidFogColor", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Z"));
                instructions.insert(insn, new VarInsnNode(ALOAD, 7));
                instructions.insert(insn, new VarInsnNode(ALOAD, 2));
                instructions.insert(insn, new VarInsnNode(ALOAD, 1));
                removeFrom(instructions, insn, -1);
            }
            /*
             * getFogColor: (changes are around line 552)
             * Old code:
             * if (state.getBlock() instanceof BlockLiquid)
             * {
             *     ...
             * }
             *
             * New code:
             * //remove unused if statement
             * if (false)
             * {
             *     ...
             * }
             */
            else if(insn.getOpcode() == INSTANCEOF) {
                instructions.insert(insn, new InsnNode(ICONST_0));
                removeFrom(instructions, insn, -2);
            }
            /*
             * getFogColor: (changes are around line 557)
             * Old code:
             * if (viewport.y > (double)f1)
             * {
             *     ...
             * }
             *
             * New code:
             * //remove other unused if statement
             * if (true)
             * {
             *     ...
             * }
             */
            else if(insn.getOpcode() == IFLE) {
                removeFrom(instructions, insn.getPrevious(), -4);
                instructions.insertBefore(insn, new InsnNode(ICONST_1));
                ((JumpInsnNode)insn).setOpcode(IFEQ);
            }
            /*
             * getFogColor: (changes are from lines 559 to 561)
             * Old code:
             * BlockPos upPos = pos.up();
             * IBlockState upState = world.getBlockState(upPos);
             * return upState.getBlock().getFogColor(world, upPos, upState, entity, originalColor, partialTicks);
             *
             * New code:
             * //do nothing if the entity isn't within the fluid
             * return originalColor;
             */
            else if(checkMethod(insn, "getFogColor")) {
                instructions.insert(insn, new VarInsnNode(ALOAD, 5));
                removeFrom(instructions, insn, -20);
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid");
        /*
         * createBlockState:
         * New code:
         * //use forge unlisted fluid props
         * protected BlockStateContainer createBlockState()
         * {
         *     return Hooks.createLiquidBlockState(this);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180661_e" : "createBlockState"),
            "createLiquidBlockState", "(Lnet/minecraft/block/Block;)Lnet/minecraft/block/state/BlockStateContainer;",
                generator -> generator.visitVarInsn(ALOAD, 0)
        );
        /*
         * shouldSideBeRendered:
         * New code:
         * //better side rendering
         * @SideOnly(Side.CLIENT)
         * public boolean shouldSideBeRendered(IBlockState blockState, IBlockAccess blockAccess, BlockPos pos, EnumFacing side)
         * {
         *     return Hooks.shouldLiquidSideBeRendered(blockState, blockAccess, pos, side);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"),
            "shouldLiquidSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );
        /*
         * getFlow:
         * New code:
         * //fix canFluidFlow-related vector bugs
         * public Vec3d getFlow(IBlockAccess worldIn, BlockPos pos, IBlockState state)
         * {
         *     return Hooks.getFlow(this, worldIn, pos, state);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_189543_a" : "getFlow"),
            "getFlow", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        //use the correct block liquid height
        overrideMethod(classNode, method -> checkMethod(method, obfuscated ? "func_190973_f" : "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F"),
            "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        //fix corner heights
        addMethod(classNode, "getExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;",
            "getFluidExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;IIFFF)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitInsn(ICONST_M1);
                generator.visitLdcInsn(8);
                generator.visitLdcInsn(8f);
                generator.visitLdcInsn(8f/9);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getSlopeAngle", "(Lnet/minecraft/block/BlockLiquid;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)D", false);
                generator.visitInsn(D2F);
            }
        );
        //getStateAtViewpoint
        addMethod(classNode, "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;",
            "getStateAtViewpoint", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );
        //isEntityInsideMaterial
        addMethod(classNode, "isEntityInsideMaterial", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;",
            "isEntityInsideFluid", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;DLnet/minecraft/block/material/Material;Z)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(DLOAD, 5);
                generator.visitVarInsn(ALOAD, 7);
                generator.visitVarInsn(ILOAD, 8);
            }
        );
        //isAABBInsideMaterial
        addMethod(classNode, "isAABBInsideMaterial", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;",
            "isAABBInsideMaterial", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );
        //isAABBInsideLiquid
        addMethod(classNode, "isAABBInsideLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "isWithinFluid", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Z", false);
            generator.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
        });
        //getFluid
        addMethod(classNode, "getFluid", "()Lnet/minecraftforge/fluids/Fluid;",
            "getLiquid", "(Lnet/minecraft/block/material/Material;)Lnet/minecraftforge/fluids/Fluid;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
            }
        );
        //place
        addMethod(classNode, "place", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;Z)I",
            "place", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;ZLnet/minecraft/block/state/IBlockState;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraft/block/Block", obfuscated ? "field_149764_J" : "material", "Lnet/minecraft/block/material/Material;");
                generator.visitMethodInsn(INVOKESTATIC, "net/minecraft/block/BlockLiquid", obfuscated ? "func_176361_a" : "getFlowingBlock", "(Lnet/minecraft/block/material/Material;)Lnet/minecraft/block/BlockDynamicLiquid;", false);
                generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false);
            }
        );
        //drain
        addMethod(classNode, "drain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Z)Lnet/minecraftforge/fluids/FluidStack;",
            "drain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitInsn(ACONST_NULL);
            }
        );
        //canDrain
        addMethod(classNode, "canDrain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "canDrain", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        //getFilledPercentage
        addMethod(classNode, "getFilledPercentage", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)F", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, "git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", false);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "getBlockLiquidHeight", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F", false);
        });
        //isFluidloggableFluid
        addMethod(classNode, "isFluidloggableFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "isLiquidFluidloggable", "(Lgit/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
            }
        );
        //isFluidloggableFluid
        addMethod(classNode, "isFluidloggableFluid", "()Z",
            "isLiquidFluidloggable", "(Lnet/minecraft/block/Block;)Z",
                generator -> generator.visitVarInsn(ALOAD, 0)
        );

        return true;
    }

    @Override
    public boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) {
        method.localVariables.add(new LocalVariableNode("here", "Lnet/minecraft/block/state/IBlockState;", null, start, end, 9));
        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static BlockStateContainer createLiquidBlockState(@Nonnull Block block) {
            return new BlockStateContainer.Builder(block)
                    .add(BlockFluidBase.FLUID_RENDER_PROPS.toArray(new IUnlistedProperty<?>[0]))
                    .add(BlockLiquid.LEVEL).build();
        }

        @Nonnull
        public static Vec3d getFlow(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here) {
            final int decay = 8 - getEffectiveQuanta(block, world, pos);
            Vec3d vec = Vec3d.ZERO;

            for(EnumFacing facing : HORIZONTALS) {
                if(canFluidFlow(world, pos, here, facing)) {
                    BlockPos offset = pos.offset(facing);

                    if(canFluidFlow(world, offset, world.getBlockState(offset), facing.getOpposite())) {
                        int otherDecay = 8 - getEffectiveQuanta(block, world, offset);

                        if(otherDecay >= 8) {
                            otherDecay = 8 - getEffectiveQuanta(block, world, offset.down());

                            if(otherDecay < 8) {
                                int power = otherDecay - (decay - 8);
                                vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                            }
                        }
                        else {
                            int power = otherDecay - decay;
                            vec = vec.add(facing.getXOffset() * power, 0, facing.getZOffset() * power);
                        }
                    }
                }
            }

            return vec.normalize();
        }

        //helper
        public static int getEffectiveQuanta(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            int quantaValue = getQuantaValue(block, world, pos);
            return quantaValue > 0 && quantaValue < 8 && PluginBlockFluidBase.Hooks.hasVerticalFlow(world, pos, block.getFluid(), -1) ? 8 : quantaValue;
        }

        //helper
        public static int getQuantaValue(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final IBlockState state = world.getBlockState(pos);
            if(state.getBlock().isAir(state, world, pos)) return 0;

            final FluidState fluidState = getFluidState(world, pos, state);
            if(!isCompatibleFluid(fluidState.getFluid(), block.getFluid())) return -1;

            final int level = fluidState.getLevel();
            return level >= 8 ? 8 : 8 - level;
        }

        public static boolean getLiquidFogColor(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull Vec3d viewport) {
            return !PluginBlockFluidBase.Hooks.isWithinFluid(state.getBlock(), (IExtendedBlockState)state.getBlock().getExtendedState(state, world, pos), world, pos, viewport);
        }

        public static boolean isLiquidFluidloggable(@Nonnull IFluidloggableFluid block, @Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos) {
            if(!block.isFluidloggableFluid()) return false;
            else if(fluid.getValue(BlockLiquid.LEVEL) == 0) return true;
            else if(fluid.getMaterial() != Material.WATER) return false;

            final IBlockState vertical = world.getBlockState(pos.up());
            return isCompatibleFluid(block.getFluid(), getFluidState(world, pos.up(), vertical).getFluid())
                    && canFluidFlow(world, pos.up(), vertical, DOWN);
        }

        public static boolean isLiquidFluidloggable(@Nonnull Block block) {
            //most modded BlockLiquid instances involve blocks that shouldn't be fluidloggable fluids (like coral)
            return block == Blocks.WATER || block == Blocks.LAVA || block == Blocks.FLOWING_WATER || block == Blocks.FLOWING_LAVA;
        }

        public static boolean shouldLiquidSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side) {
            return PluginBlockFluidBase.Hooks.shouldFluidSideBeRendered(state, world, pos, side, -1);
        }
    }
}

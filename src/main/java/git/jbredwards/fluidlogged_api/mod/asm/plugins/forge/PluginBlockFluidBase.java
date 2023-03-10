package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import com.google.common.collect.ImmutableMap;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.property.IExtendedBlockState;
import net.minecraftforge.common.property.IUnlistedProperty;
import net.minecraftforge.common.property.PropertyFloat;
import net.minecraftforge.fluids.BlockFluidBase;
import net.minecraftforge.fluids.Fluid;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;
import static net.minecraft.util.EnumFacing.*;

/**
 * modded fluids work properly with the mod & prevent startup crash
 * @author jbred
 *
 */
public final class PluginBlockFluidBase implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "<init>", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/material/Material;Lnet/minecraft/block/material/MapColor;)V")) return 1;
        else if(checkMethod(method, "<clinit>", "()V")) return 2;
        else if(method.name.equals("canDisplace")) return 3;
        else if(method.name.equals("getFlowDirection") || method.name.equals("getDensity") || method.name.equals("getTemperature")) return 4;
        else if(method.name.equals("getFluid")) return 5;
        else if(checkMethod(method, "getFilledPercentage", "(Lnet/minecraft.world.IBlockAccess;Lnet/minecraft/util/math/BlockPos;)F")) return 6;
        else return method.name.equals("getStateAtViewpoint") ? 7 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * A bit of context, this mod makes BlockLiquid implement several IUnlistedProperties, and since
         * those properties are initialized in forge's BlockFluidBase class, forge's class gets loaded early.
         * This means that the blocks defined in BlockFluidBase#defaultDisplacements aren't initialized yet,
         * which causes the game to crash. This transformation moves the defaultDisplacements initialization
         * from a static initializer to one in the class's constructor. This transformer insures the game
         * doesn't crash on startup.
         */
        if(index == 1 && checkField(insn, "defaultDisplacements", "Ljava/util/Map;")) {
            instructions.insert(insn, genMethodNode("defaultDisplacements", "(Ljava/util/Map;)Ljava/util/Map;"));
            return true;
        }
        /*
         * Removes the static initializer for BlockFluidBase#defaultPlacements, reason mentioned above.
         */
        else if(index == 2 && insn.getNext().getOpcode() == LDC) {
            //removes all default entries, as the class is now loaded before they're registered
            while(insn.getPrevious().getOpcode() != PUTSTATIC) instructions.remove(insn.getPrevious());
            return true;
        }
        //canDisplace:
        else if(index == 3) {
            /*
             * (changes are around line 284)
             * Old code:
             * if (block == this)
             * {
             *     ....
             * }
             *
             * New code:
             * //checks if the FluidState is compatible with this fluid
             * if (FluidloggedUtils.isCompatibleFluid(this.getFluid(), FluidloggedUtils.getFluidState(world, pos, state).getFluid()))
             * {
             *     ....
             * }
             */
            if(insn.getOpcode() == IF_ACMPNE) {
                final InsnList list = new InsnList();
                list.add(new MethodInsnNode(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidBase", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", false));
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 2));
                list.add(new VarInsnNode(ALOAD, 3));
                list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidState", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;)Lgit/jbredwards/fluidlogged_api/api/util/FluidState;"));
                list.add(new MethodInsnNode(INVOKEVIRTUAL, "git/jbredwards/fluidlogged_api/api/util/FluidState", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", false));
                list.add(genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "isCompatibleFluid", "(Lnet/minecraftforge/fluids/Fluid;Lnet/minecraftforge/fluids/Fluid;)Z"));
                //remove old
                instructions.remove(getPrevious(insn, 2));
                instructions.insertBefore(insn, list);
                ((JumpInsnNode)insn).setOpcode(IFEQ);
            }
            /*
             * (changes are around line 295)
             * Old code:
             * if (material.blocksMovement() || material == Material.PORTAL || material == Material.STRUCTURE_VOID)
             * {
             *     ....
             * }
             *
             * New code:
             * //ensure the block here does not use Material.CIRCUITS
             * if (material.blocksMovement() || material == Material.PORTAL || material == Material.CIRCUITS || material == Material.STRUCTURE_VOID)
             * {
             *     ....
             * }
             */
            else if(insn.getOpcode() == IF_ACMPEQ) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 5));
                list.add(new FieldInsnNode(GETSTATIC, "net/minecraft/block/material/Material", obfuscated ? "field_151594_q" : "CIRCUITS", "Lnet/minecraft/block/material/Material;"));
                list.add(new JumpInsnNode(IF_ACMPEQ, ((JumpInsnNode)insn).label));
                instructions.insert(insn, list);
                return true;
            }
        }
        /*
         * getFlowDirection, getDensity, and getTemperature: (changes are around lines 653, 594, and 612)
         * Old code:
         * IBlockState state = world.getBlockState(pos);
         *
         * New code:
         * //ensure that FluidStates render their flow vectors
         * IBlockState state = FluidloggedUtils.getFluidOrReal(world, pos);
         */
        else if(index == 4 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * getFluid: (changes are around line 810)
         * Old code:
         * return FluidRegistry.getFluid(fluidName);
         *
         * New code:
         * //exposed to significantly boost performance, sorry forge devs but why'd you not want this exposed again?
         * return this.definedFluid;
         */
        else if(index == 5 && checkMethod(insn, "getFluid", "(Ljava/lang/String;)Lnet/minecraftforge/fluids/Fluid;")) {
            instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;"));
            instructions.remove(insn.getPrevious());
            instructions.remove(insn);
            return true;
        }
        /*
         * getFilledPercentage: (changes are around line 823)
         * Old code:
         * return remaining * (density > 0 ? 1 : -1);
         *
         * New code:
         * //fixes a general inaccuracy with modded fluids (this especially comes up in other mods like Biomes O'Plenty)
         * return this.quantaFraction * remaining * (density > 0 ? 1 : -1);
         */
        else if(index == 6 && insn.getOpcode() == FMUL) {
            instructions.insertBefore(insn, new InsnNode(FMUL));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
            instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F"));
            return true;
        }
        /*
         * getStateAtViewpoint: (changes are around line 864)
         * Old code:
         * return world.getBlockState(pos.down(this.densityDir));
         *
         * New code:
         * //return the other block here if the player isn't within the fluid
         * return Hooks.getStateAtViewpoint(world, pos, state, viewpoint);
         */
        else if(index == 7 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            instructions.insert(insn, genMethodNode("getStateAtViewpoint", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/math/Vec3d;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.insert(insn, new VarInsnNode(ALOAD, 4));
            instructions.insert(insn, new VarInsnNode(ALOAD, 1));
            removeFrom(instructions, insn, -3);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * shouldSideBeRendered:
         * New code:
         * //this does a few things differently:
         * //1. check for neighboring FluidStates rather than neighboring IBlockStates
         * //2. check for compatible fluids rather than just ones with an identical Material
         * //3. check for neighboring fluids this is connected to
         * @Override
         * public boolean shouldSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side)
         * {
         *     return Hooks.shouldFluidSideBeRendered(state, world, pos, this.densityDir);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"),
            "shouldFluidSideBeRendered", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
            }
        );
        /*
         * getExtendedState:
         * New code:
         * //fix corner heights and some vanilla 1.13+ inconsistencies
         * @Override
         * @Nonnull
         * public IBlockState getExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos)
         * {
         *     return Hooks.getFluidExtendedState(oldState, world, pos, this.getFluid(), this.densityDir, this.quantaPerBlock, this.quantaPerBlockFloat, this.quantaFraction, (float)getFlowDirection(world, pos));
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getExtendedState"),
            "getFluidExtendedState", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;IIFFF)Lnet/minecraft/block/state/IBlockState;", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlockFloat", "F");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaFraction", "F");
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitMethodInsn(INVOKESTATIC, "net/minecraftforge/fluids/BlockFluidBase", "getFlowDirection", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)D", false);
                generator.visitInsn(D2F);
            }
        );
        /*
         * getFlowVector:
         * New code:
         * //don't flow into/from invalid sides
         * public Vec3d getFlowVector(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.getFluidFlowVector(this, world, pos, this.densityDir, this.quantaPerBlock);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getFlowVector"),
            "getFluidFlowVector", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;II)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
            }
        );
        /*
         * hasVerticalFlow:
         * New code:
         * //don't flow into/from invalid sides
         * final boolean hasVerticalFlow(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.hasVerticalFlow(world, pos, this.getFluid(), this.densityDir);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("hasVerticalFlow"),
            "hasVerticalFlow", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraftforge/fluids/IFluidBlock", "getFluid", "()Lnet/minecraftforge/fluids/Fluid;", true);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
            }
        );
        /*
         * getFogColor:
         * New code:
         * //remove built-in checks for better ones
         * @Override
         * @SideOnly (Side.CLIENT)
         * public Vec3d getFogColor(World world, BlockPos pos, IBlockState state, Entity entity, Vec3d originalColor, float partialTicks)
         * {
         *     return Hooks.getFluidFogColor(this, world, pos, state, entity, originalColor, partialTicks);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getFogColor"),
            "getFluidFogColor", "(Lnet/minecraftforge/fluids/BlockFluidBase;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/entity/Entity;Lnet/minecraft/util/math/Vec3d;F)Lnet/minecraft/util/math/Vec3d;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
                generator.visitVarInsn(ALOAD, 5);
                generator.visitVarInsn(FLOAD, 6);
            }
        );
        /*
         * isEntityInsideMaterial:
         * New code:
         * //better entity fluid collision
         * @ASMGenerated
         * public Boolean isEntityInsideMaterial(IBlockAccess world, BlockPos blockpos, IBlockState iblockstate, Entity entity, double yToTest, Material materialIn, boolean testingHead)
         * {
         *     return Hooks.isEntityInsideFluid(this, world, blockpos, iblockstate, entity, yToTest, materialIn, testingHead);
         * }
         */
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
        /*
         * isAABBInsideMaterial:
         * New code:
         * //better entity fluid collision
         * @ASMGenerated
         * public Boolean isAABBInsideMaterial(World world, BlockPos pos, AxisAlignedBB boundingBox, Material materialIn)
         * {
         *     return Hooks.isAABBInsideMaterial(this, world, pos, boundingBox, materialIn);
         * }
         */
        addMethod(classNode, "isAABBInsideMaterial", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;",
            "isAABBInsideMaterial", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;Lnet/minecraft/block/material/Material;)Ljava/lang/Boolean;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 4);
            }
        );
        /*
         * isAABBInsideLiquid:
         * New code:
         * //better entity fluid collision
         * @ASMGenerated
         * public Boolean isAABBInsideLiquid(World world, BlockPos pos, AxisAlignedBB boundingBox)
         * {
         *     return Boolean.valueOf(Hooks.isWithinFluid(this, world, pos, boundingBox));
         * }
         */
        addMethod(classNode, "isAABBInsideLiquid", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Ljava/lang/Boolean;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "isWithinFluid", "(Lnet/minecraft/block/Block;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/AxisAlignedBB;)Z", false);
            generator.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
        });
        /*
         * Accessor:
         * New code:
         * //add public accessor for private method
         * @ASMGenerated
         * public int getFlowDecay_Public(@Nonnull IBlockAccess world, @Nonnull BlockPos pos)
         * {
         *     return this.getFlowDecay(world, pos);
         * }
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidBase$Accessor");
        addMethod(classNode, "getFlowDecay_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKESPECIAL, "net/minecraftforge/fluids/BlockFluidBase", "getFlowDecay", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)I", false);
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static Map<Block, Boolean> defaultDisplacements(@Nonnull Map<Block, Boolean> map) {
            final Map<Block, Boolean> ret = new HashMap<>();
            //restore old entries
            ret.put(Blocks.OAK_DOOR,                       false);
            ret.put(Blocks.SPRUCE_DOOR,                    false);
            ret.put(Blocks.BIRCH_DOOR,                     false);
            ret.put(Blocks.JUNGLE_DOOR,                    false);
            ret.put(Blocks.ACACIA_DOOR,                    false);
            ret.put(Blocks.DARK_OAK_DOOR,                  false);
            ret.put(Blocks.TRAPDOOR,                       false);
            ret.put(Blocks.IRON_TRAPDOOR,                  false);
            ret.put(Blocks.OAK_FENCE,                      false);
            ret.put(Blocks.SPRUCE_FENCE,                   false);
            ret.put(Blocks.BIRCH_FENCE,                    false);
            ret.put(Blocks.JUNGLE_FENCE,                   false);
            ret.put(Blocks.DARK_OAK_FENCE,                 false);
            ret.put(Blocks.ACACIA_FENCE,                   false);
            ret.put(Blocks.NETHER_BRICK_FENCE,             false);
            ret.put(Blocks.OAK_FENCE_GATE,                 false);
            ret.put(Blocks.SPRUCE_FENCE_GATE,              false);
            ret.put(Blocks.BIRCH_FENCE_GATE,               false);
            ret.put(Blocks.JUNGLE_FENCE_GATE,              false);
            ret.put(Blocks.DARK_OAK_FENCE_GATE,            false);
            ret.put(Blocks.ACACIA_FENCE_GATE,              false);
            ret.put(Blocks.WOODEN_PRESSURE_PLATE,          false);
            ret.put(Blocks.STONE_PRESSURE_PLATE,           false);
            ret.put(Blocks.LIGHT_WEIGHTED_PRESSURE_PLATE,  false);
            ret.put(Blocks.HEAVY_WEIGHTED_PRESSURE_PLATE,  false);
            ret.put(Blocks.LADDER,                         false);
            ret.put(Blocks.IRON_BARS,                      false);
            ret.put(Blocks.GLASS_PANE,                     false);
            ret.put(Blocks.STAINED_GLASS_PANE,             false);
            ret.put(Blocks.PORTAL,                         false);
            ret.put(Blocks.END_PORTAL,                     false);
            ret.put(Blocks.COBBLESTONE_WALL,               false);
            ret.put(Blocks.BARRIER,                        false);
            ret.put(Blocks.STANDING_BANNER,                false);
            ret.put(Blocks.WALL_BANNER,                    false);
            ret.put(Blocks.CAKE,                           false);
            ret.put(Blocks.IRON_DOOR,                      false);
            ret.put(Blocks.STANDING_SIGN,                  false);
            ret.put(Blocks.WALL_SIGN,                      false);
            ret.put(Blocks.REEDS,                          false);
            //new entries added by other mods (never actually seen mods do this, but just in case)
            ret.putAll(map);

            return ret;
        }

        @Nonnull
        public static IBlockState getFluidExtendedState(@Nonnull IBlockState oldState, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int densityDir, int quantaPerBlock, float quantaPerBlockFloat, float quantaFraction, float flowDirection) {
            if(!(oldState instanceof IExtendedBlockState)) return oldState;
            final EnumFacing densityFace = densityDir < 0 ? UP : DOWN;

            //covert to extended state
            IExtendedBlockState state = new FluidExtendedBlockState((IExtendedBlockState)oldState);
            state = state.withProperty(BlockFluidBase.FLOW_DIRECTION, flowDirection);

            //corner height variables
            final IBlockState[][][] states = new IBlockState[3][2][3];
            final FluidState[][][] fluids = new FluidState[3][2][3];
            final float[][] height = new float[3][3];
            final float[][] corner = new float[2][2];

            //initialize here states
            states[1][0][1] = world.getBlockState(pos);
            states[1][1][1] = world.getBlockState(pos.down(densityDir));
            fluids[1][0][1] = getFluidState(world, pos, states[1][0][1]);
            fluids[1][1][1] = getFluidState(world, pos.down(densityDir), states[1][1][1]);
            height[1][1] = getFluidHeightForRender(world, pos, states, fluids, 1, 1, densityFace, quantaPerBlock, quantaPerBlockFloat, quantaFraction);

            //fluid block above this
            if(height[1][1] == 1)
                for(int i = 0; i < 2; i++)
                    for(int j = 0; j < 2; j++)
                        corner[i][j] = 1;
            //no fluid block above this
            else {
                //get corner heights from all 8 sides
                for(int i = 0; i < 3; i++) {
                    for(int j = 0; j < 3; j++) {
                        if(i != 1 || j != 1) {
                            if(states[i][0][j] == null) states[i][0][j] = world.getBlockState(pos.add(i - 1, 0, j - 1));
                            if(states[i][1][j] == null) states[i][1][j] = world.getBlockState(pos.add(i - 1, -densityDir, j - 1));
                            if(fluids[i][0][j] == null) fluids[i][0][j] = getFluidState(world, pos.add(i - 1, 0, j - 1), states[i][0][j]);
                            if(fluids[i][1][j] == null) fluids[i][1][j] = getFluidState(world, pos.add(i - 1, -densityDir, j - 1), states[i][1][j]);
                            height[i][j] = getFluidHeightForRender(world, pos, states, fluids, i, j, densityFace, quantaPerBlock, quantaPerBlockFloat, quantaFraction);
                        }
                    }
                }
                //find average of gathered heights for each corner
                for(int i = 0; i < 2; i++)
                    for(int j = 0; j < 2; j++)
                        corner[i][j] = getFluidHeightAverage(quantaFraction, height[i][j], height[i][j + 1], height[i + 1][j], height[i + 1][j + 1]);
            }

            //side overlays, skipped if there's no overlay texture
            if(fluid.getOverlay() != null) {
                for(int i = 0; i < 4; i++) {
                    EnumFacing side = byHorizontalIndex(i);
                    BlockPos offset = pos.offset(side);
                    //use cache if available
                    state = state.withProperty(BlockFluidBase.SIDE_OVERLAYS[i], !canFluidFlow(world, offset,
                            getOrSet(states, () -> world.getBlockState(offset), side.getXOffset() + 1, side.getZOffset() + 1),
                            side.getOpposite()));
                }
            }

            //fix possible top z fighting
            if(!canFluidFlow(world, pos, states[1][0][1], densityFace)) {
                if(corner[0][0] == 1) corner[0][0] = 0.998f;
                if(corner[0][1] == 1) corner[0][1] = 0.998f;
                if(corner[1][0] == 1) corner[1][0] = 0.998f;
                if(corner[1][1] == 1) corner[1][1] = 0.998f;
            }

            //sets the corner props
            state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[0], corner[0][0], quantaFraction);
            state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[1], corner[0][1], quantaFraction);
            state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[2], corner[1][1], quantaFraction);
            state = withPropertyFallback(state, BlockFluidBase.LEVEL_CORNERS[3], corner[1][0], quantaFraction);
            return state;
        }

        //helper
        @Nonnull
        static final EnumFacing[][] FACES = {
                {NORTH, WEST}, {NORTH}, {NORTH, EAST},
                {WEST},    {     },     {EAST},
                {SOUTH, WEST}, {SOUTH}, {SOUTH, EAST},
        };

        //helper
        public static float getFluidHeightForRender(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState[][][] states, @Nonnull FluidState[][][] fluids, int i, int j, EnumFacing densityFace, int quantaPerBlock, float quantaPerBlockFloat, float quantaFraction) {
            final EnumFacing[] faces = FACES[j * 3 + i];

            //check for vertical connections
            if(connectToVertical(states, fluids, world, pos, densityFace, i, j, faces))
                return 1;

            //check for horizontal connections
            if(connectToHorizontal(states, fluids, world, pos, i, j, faces)) {
                //air block
                if(states[i][0][j].getBlock().isAir(states[i][0][j], world, pos.add(i - 1, 0, j - 1)))
                    return 0;

                //fluid block
                else if(isCompatibleFluid(fluids[1][0][1].getFluid(), fluids[i][0][j].getFluid())) {
                    int lowest = capLowest(fluids[i][0][j].getLevel());
                    if(lowest == 0) return quantaFraction;

                    //diagonals check if nearby have lower levels to connect
                    if(faces.length > 1) {
                        for(EnumFacing facing : faces) {
                            EnumFacing other = faces[facing.getXOffset() != 0 ? 0 : 1];
                            BlockPos offset = pos.offset(facing);
                            IBlockState neighbor = getOrSet(states, () -> world.getBlockState(offset), facing);

                            //check indirect adjacent
                            if((!canFluidFlow(world, pos, states[1][0][1], other)
                            || !canFluidFlow(world, pos.offset(other), getOrSet(states, () -> world.getBlockState(pos.offset(other)), other), other.getOpposite()))
                            && canFluidFlow(world, pos, states[1][0][1], facing)
                            && canFluidFlow(world, offset, neighbor, facing.getOpposite())
                            && canFluidFlow(world, offset, neighbor, other)
                            && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], other.getOpposite())
                            && isCompatibleFluid(fluids[1][0][1].getFluid(), getOrSet(fluids, () -> getFluidState(world, offset, neighbor), facing).getFluid())
                            && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], facing.getOpposite())
                            && canFluidFlow(world, pos.offset(other), getOrSet(states, () -> world.getBlockState(pos.offset(other)), other), facing)
                            && isCompatibleFluid(fluids[1][0][1].getFluid(), getOrSet(fluids, () -> getFluidState(world, pos.offset(other), get(states, other)), other).getFluid())
                            && lowest > get(fluids, other).getLevel()) {
                                lowest = capLowest(get(fluids, other).getLevel());
                                if(lowest == 0) break;
                            }
                        }
                    }

                    //connect to neighbor fluid at the correct height
                    return ((quantaPerBlock - lowest) / quantaPerBlockFloat) * quantaFraction;
                }
            }

            //default
            return -1;
        }

        //helper
        public static int capLowest(int lowest) { return lowest >= 8 ? 0 : lowest; }

        //helper
        public static float getFluidHeightAverage(float quantaFraction, @Nonnull float... heights) {
            float total = 0;
            int count = 0;

            for(float height : heights) {
                if(height == 1) //vertical fluid
                    return 1;

                if(height >= quantaFraction) {
                    total += height * 10;
                    count += 10;
                }

                if(height >= 0) {
                    total += height;
                    count++;
                }
            }

            return total / count;
        }

        //helper
        public static boolean connectToVertical(@Nonnull IBlockState[][][] states, @Nonnull FluidState[][][] fluids, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing densityFace, int i, int j, @Nonnull EnumFacing[] sides) {
            return isCompatibleFluid(fluids[1][0][1].getFluid(), fluids[i][1][j].getFluid())
                    && isCompatibleFluid(fluids[1][0][1].getFluid(), fluids[i][0][j].getFluid())
                    && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], densityFace)
                    && canFluidFlow(world, pos.add(i - 1, -densityFace.getYOffset(), j - 1), states[i][1][j], densityFace.getOpposite())
                    && connectToHorizontal(states, fluids, world, pos, i, j, sides);
        }

        //helper
        public static boolean connectToHorizontal(@Nonnull IBlockState[][][] states, @Nonnull FluidState[][][] fluids, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int i, int j, @Nonnull EnumFacing[] sides) {
            final boolean diagonal = sides.length > 1;
            for(EnumFacing facing : sides) {
                //check diagonal
                if(diagonal) {
                    EnumFacing other = sides[facing.getXOffset() != 0 ? 0 : 1];
                    BlockPos offset = pos.offset(facing);
                    IBlockState neighbor = getOrSet(states, () -> world.getBlockState(offset), facing);

                    if(canFluidFlow(world, pos, states[1][0][1], facing)
                            && canFluidFlow(world, offset, neighbor, facing.getOpposite())
                            && canFluidFlow(world, offset, neighbor, other)
                            && canFluidFlow(world, pos.add(i - 1, 0, j - 1), states[i][0][j], other.getOpposite())
                            && isCompatibleFluid(fluids[1][0][1].getFluid(), getOrSet(fluids, () -> getFluidState(world, offset, neighbor), facing).getFluid()))

                        return true;
                }
                //check adjacent
                else if(!canFluidFlow(world, pos, states[1][0][1], facing) || !canFluidFlow(world, pos.offset(facing), states[i][0][j], facing.getOpposite()))
                    return false;
            }

            return !diagonal;
        }

        //helper
        @Nonnull
        public static <T> T get(@Nonnull T[][][] values, @Nonnull EnumFacing facing) { return values[facing.getXOffset() + 1][0][facing.getZOffset() + 1]; }

        //helper
        @Nonnull
        public static <T> T getOrSet(@Nonnull T[][][] values, @Nonnull Supplier<T> fallback, @Nonnull EnumFacing facing) {
            return getOrSet(values, fallback, facing.getXOffset() + 1, facing.getZOffset() + 1);
        }

        //helper
        @Nonnull
        public static <T> T getOrSet(@Nonnull T[][][] values, @Nonnull Supplier<T> fallback, int i, int j) {
            if(values[i][0][j] != null) return values[i][0][j];
            else return values[i][0][j] = fallback.get();
        }

        //helper
        @Nonnull
        public static IExtendedBlockState withPropertyFallback(@Nonnull IExtendedBlockState state, @Nonnull PropertyFloat property, float value, float quantaFraction) {
            return state.withProperty(property, property.isValid(value) ? value : quantaFraction);
        }

        @Nonnull
        public static Vec3d getFluidFlowVector(@Nonnull BlockFluidBase block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int densityDir, int quantaPerBlock) {
            final IBlockState here = world.getBlockState(pos);
            Vec3d vec = Vec3d.ZERO;

            final int decay = ((Accessor)block).getFlowDecay_Public(world, pos);
            for(EnumFacing facing : HORIZONTALS) {
                if(canFluidFlow(world, pos, here, facing)) {
                    BlockPos offset = pos.offset(facing);
                    if(canFluidFlow(world, offset, world.getBlockState(offset), facing.getOpposite())) {
                        int otherDecay = ((Accessor)block).getFlowDecay_Public(world, offset);

                        if(otherDecay >= quantaPerBlock) {
                            otherDecay = ((Accessor)block).getFlowDecay_Public(world, offset.up(densityDir));

                            if(otherDecay < quantaPerBlock) {
                                int power = otherDecay - (decay - quantaPerBlock);
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

        @Nonnull
        public static Vec3d getFluidFogColor(@Nonnull BlockFluidBase block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entity, @Nonnull Vec3d originalColor, float partialTicks) {
            //remove built-in in place for better check
            if(isWithinFluid(block, (IExtendedBlockState)block.getExtendedState(state, world, pos), world, pos, ActiveRenderInfo.projectViewFromEntity(entity, partialTicks))) {
                int color = block.getFluid().getColor();
                float red = (color >> 16 & 0xFF) / 255.0f;
                float green = (color >> 8 & 0xFF) / 255.0f;
                float blue = (color & 0xFF) / 255.0f;
                return new Vec3d(red, green, blue);
            }

            //not inside fluid
            return originalColor;
        }

        @Nonnull
        public static IBlockState getStateAtViewpoint(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Vec3d viewpoint) {
            final IBlockState here = world.getBlockState(pos);
            return here == state ? Blocks.AIR.getDefaultState()
                    : here.getBlock().getStateAtViewpoint(here, world, pos, viewpoint);
        }

        public static boolean hasVerticalFlow(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, int densityDir) {
            final EnumFacing facing = (densityDir < 0) ? UP : DOWN;
            if(!canFluidFlow(world, pos, world.getBlockState(pos), facing)) return false;

            final BlockPos offset = pos.down(densityDir);
            final IBlockState state = world.getBlockState(offset);

            return canFluidFlow(world, offset, state, facing.getOpposite())
                    && isCompatibleFluid(getFluidState(world, offset, state).getFluid(), fluid);
        }

        @Nullable
        public static Boolean isAABBInsideMaterial(@Nonnull Block block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB boundingBox, @Nonnull Material materialIn) {
            return materialIn != block.getDefaultState().getMaterial() ? null : block.isAABBInsideLiquid(world, pos, boundingBox);
        }

        @Nullable
        public static Boolean isEntityInsideFluid(@Nonnull Block block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Entity entity, double yToTest, @Nonnull Material materialIn, boolean testingHead) {
            if(materialIn != state.getMaterial() || !(state instanceof IExtendedBlockState)) return null;
            else if(!testingHead) return isWithinFluid(block, world, pos, entity.getEntityBoundingBox());
            return isWithinFluid(block, (IExtendedBlockState)block.getExtendedState(state, world, pos), world, pos, new Vec3d(entity.posX, yToTest, entity.posZ));
        }

        public static boolean isWithinFluid(@Nonnull Block block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull AxisAlignedBB bb) {
            bb = bb.intersect(new AxisAlignedBB(pos.getX(), pos.getY(), pos.getZ(), pos.getX() + 1, pos.getY() + 1, pos.getZ() + 1));

            final IBlockState state = block.getExtendedState(getFluidOrReal(world, pos), world, pos);
            if(!(state instanceof IExtendedBlockState)) return true;

            final IExtendedBlockState extendedState = (IExtendedBlockState)state;
            return isWithinFluid(block, extendedState, world, pos, new Vec3d(bb.minX, bb.minY, bb.minZ))
                    || isWithinFluid(block, extendedState, world, pos, new Vec3d(bb.minX, bb.minY, bb.maxZ))
                    || isWithinFluid(block, extendedState, world, pos, new Vec3d(bb.maxX, bb.minY, bb.minZ))
                    || isWithinFluid(block, extendedState, world, pos, new Vec3d(bb.maxX, bb.minY, bb.maxZ));
        }

        //helper
        public static boolean isWithinFluid(@Nonnull Block block, @Nonnull IExtendedBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull Vec3d entityVec) {
            final float[][] corners = new float[2][2];
            corners[0][0] = state.getValue(BlockFluidBase.LEVEL_CORNERS[0]);
            corners[0][1] = state.getValue(BlockFluidBase.LEVEL_CORNERS[1]);
            corners[1][1] = state.getValue(BlockFluidBase.LEVEL_CORNERS[2]);
            corners[1][0] = state.getValue(BlockFluidBase.LEVEL_CORNERS[3]);

            double x = entityVec.x - pos.getX();
            double z = entityVec.z - pos.getZ();
            if(x < 0) x++;
            if(z < 0) z++;

            final double dif1 = Math.sqrt(2) - distance(0, x, 0, z);
            final double dif2 = Math.sqrt(2) - distance(0, x, 1, z);
            final double dif3 = Math.sqrt(2) - distance(1, x, 1, z);
            final double dif4 = Math.sqrt(2) - distance(1, x, 0, z);
            final double fluidHeight = (corners[0][0] * dif1 + corners[0][1] * dif2 + corners[1][1] * dif3 + corners[1][0] * dif4)
                    / (dif1 + dif2 + dif3 + dif4);

            //if fluid is upside down, do upside down collision
            return block instanceof BlockFluidBase && ((BlockFluidBase)block).getDensity() < 0
                    ? entityVec.y > pos.getY() - fluidHeight + 1 : entityVec.y < pos.getY() + fluidHeight;
        }

        //helper
        public static double distance(double x1, double x2, double z1, double z2) {
            final double distX = Math.max(x1, x2) - Math.min(x1, x2);
            final double distZ = Math.max(z1, z2) - Math.min(z1, z2);
            return Math.sqrt(distX * distX + distZ * distZ);
        }

        public static boolean shouldFluidSideBeRendered(@Nonnull IBlockState state, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull EnumFacing side, int densityDir) {
            if(!canFluidFlow(world, pos, world.getBlockState(pos), side)) return true;
            final IBlockState neighbor = world.getBlockState(pos.offset(side));
            //this check exists for mods like coral reef that don't have proper block sides
            if(isCompatibleFluid(getFluidFromState(state), getFluidFromState(neighbor))) return false;
            else if(side != (densityDir > 0 ? DOWN : UP) && neighbor.doesSideBlockRendering(world, pos.offset(side), side.getOpposite())) return false;
            return !isCompatibleFluid(getFluidState(world, pos.offset(side), neighbor).getFluid(), getFluidFromState(state))
                    || !canFluidFlow(world, pos.offset(side), neighbor, side.getOpposite());
        }
    }

    public interface Accessor
    {
        int getFlowDecay_Public(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);
    }

    //faster version of the default IExtendedBlockState with hardcoded properties, used exclusively for rendering & collision logic
    private static final class FluidExtendedBlockState extends BlockStateContainer.StateImplementation implements IExtendedBlockState
    {
        private Float flowDirection;
        private final Float[] levelCorners = new Float[4];
        private final Boolean[] sideOverlays = new Boolean[4];

        @Nonnull
        private final IExtendedBlockState parent;
        private FluidExtendedBlockState(@Nonnull IExtendedBlockState parentIn) {
            super(parentIn.getBlock(), parentIn.getProperties());
            parent = parentIn;
        }

        @SuppressWarnings("unchecked")
        @Nonnull
        @Override
        public <V> V getValue(@Nonnull IUnlistedProperty<V> property) {
            if(property == BlockFluidBase.FLOW_DIRECTION) return (V)flowDirection;
            else if(property == BlockFluidBase.LEVEL_CORNERS[0]) return (V)levelCorners[0];
            else if(property == BlockFluidBase.LEVEL_CORNERS[1]) return (V)levelCorners[1];
            else if(property == BlockFluidBase.LEVEL_CORNERS[2]) return (V)levelCorners[2];
            else if(property == BlockFluidBase.LEVEL_CORNERS[3]) return (V)levelCorners[3];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[0]) return (V)sideOverlays[0];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[1]) return (V)sideOverlays[1];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[2]) return (V)sideOverlays[2];
            else if(property == BlockFluidBase.SIDE_OVERLAYS[3]) return (V)sideOverlays[3];

            else return parent.getValue(property); //should never be called
        }

        @Nonnull
        @Override
        public <V> IExtendedBlockState withProperty(@Nonnull IUnlistedProperty<V> property, @Nonnull V value) {
            if(property == BlockFluidBase.FLOW_DIRECTION) flowDirection = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[0]) levelCorners[0] = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[1]) levelCorners[1] = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[2]) levelCorners[2] = (Float)value;
            else if(property == BlockFluidBase.LEVEL_CORNERS[3]) levelCorners[3] = (Float)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[0]) sideOverlays[0] = (Boolean)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[1]) sideOverlays[1] = (Boolean)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[2]) sideOverlays[2] = (Boolean)value;
            else if(property == BlockFluidBase.SIDE_OVERLAYS[3]) sideOverlays[3] = (Boolean)value;

            else return parent.withProperty(property, value); //should never be called
            return this;
        }

        @Nonnull
        @Override
        public ImmutableMap<IUnlistedProperty<?>, Optional<?>> getUnlistedProperties() { return parent.getUnlistedProperties(); }

        @Nonnull
        @Override
        public Collection<IUnlistedProperty<?>> getUnlistedNames() { return parent.getUnlistedNames(); }

        @Nonnull
        @Override
        public IBlockState getClean() { return parent.getClean(); }
    }
}

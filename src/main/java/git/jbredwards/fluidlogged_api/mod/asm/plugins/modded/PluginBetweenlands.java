package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.IFluidBlock;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * the betweenlands mod overrides most forge fluid methods to implement its own sudo fluidlogging,
 * this mod fixes everything betweenlands did already, so this undoes all of its stuff
 * @author jbred
 *
 */
public final class PluginBetweenlands implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals("<init>")) return 1;
        else if(method.name.equals(obfuscated ? "func_176200_f" : "isReplaceable")) return 2;
        else return method.name.equals("place") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * Constructor:
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * //betweenlands swamp water can create sources
         * {
         *     ...
         *     this.canCreateSources = true;
         * }
         */
        if(index == 1 && insn.getOpcode() == INVOKESPECIAL) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new InsnNode(ICONST_1));
            list.add(new FieldInsnNode(PUTFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z"));
            instructions.insert(insn, list);
            return true;
        }
        /*
         * isReplaceable:
         * Old code:
         * IBlockState state = world.getBlockState(pos);
         *
         * New code:
         * //The block here will always be this (unless there's some *really* poorly coded mod out there)
         * //Since the state properties don't matter for this check, use defaultState to improve performance
         * //instead of calling FluidloggedUtils#getFluidOrReal
         * IBlockState state = this.getDefaultState();
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState")) {
            //add this.getDefaultState()
            instructions.insert(insn, new MethodInsnNode(INVOKESPECIAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false));
            instructions.insert(insn, new VarInsnNode(ALOAD, 0));
            //remove world.getBlockState(pos)
            instructions.remove(getPrevious(insn, 2));
            instructions.remove(getPrevious(insn, 1));
            instructions.remove(insn);
            return true;
        }
        /*
         * place:
         * Old code:
         * FluidUtil.destroyBlockOnFluidPlacement(world, pos);
         * world.setBlockState(pos, this.getDefaultState(), 11);
         *
         * New code:
         * //allow the betweenlands place method to fluidlog blocks if possible
         * Hooks.fixBetweenlandsPlace(world, pos, this.getDefaultState(), 11, this, state);
         */
        else if(index == 3) {
            if(checkMethod(insn, "destroyBlockOnFluidPlacement")) removeFrom(instructions, insn, -2);
            else if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 5));
                instructions.insertBefore(insn, genMethodNode("fixBetweenlandsPlace", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;ILnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/block/state/IBlockState;)Z"));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.methods.removeIf(method
                -> method.name.equals("canDisplace")
                || method.name.equals("displaceIfPossible")
                || method.name.equals("getFluidHeightForRender")
                || method.name.equals("getFlowVector")
                || method.name.equals("causesDownwardCurrent")
                || method.name.equals("getQuantaValue")
                || method.name.equals("canFlowInto")
                || method.name.equals(obfuscated ? "func_180650_b" : "updateTick")
                || method.name.equals(obfuscated ? "func_176225_a" : "shouldSideBeRendered"));

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean fixBetweenlandsPlace(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState fluid, int flags, @Nonnull IFluidBlock block, @Nonnull IBlockState here) {
            if(FluidloggedUtils.isStateFluidloggable(here, world, pos, block.getFluid()))
                return FluidloggedUtils.setFluidState(world, pos, here, FluidState.of(block.getFluid()), true, true, flags);

            else {
                FluidUtil.destroyBlockOnFluidPlacement(world, pos);
                return world.setBlockState(pos, fluid, flags);
            }
        }
    }
}

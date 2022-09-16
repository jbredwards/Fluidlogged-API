package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.wrappers.FluidBlockWrapper;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * changes some of this class's util functions to be FluidState sensitive
 * @author jbred
 *
 */
public final class PluginFluidUtil implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "getFluidHandler", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/capability/IFluidHandler;")) return 1;
        else if(checkMethod(method, "tryPickUpFluid", "(Lnet/minecraft/item/ItemStack;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/EnumFacing;)Lnet/minecraftforge/fluids/FluidActionResult;")) return 2;
        return checkMethod(method, "tryPlaceFluid", "(Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/capability/IFluidHandler;Lnet/minecraftforge/fluids/FluidStack;)Z") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getFluidHandler: (changes are around line 543)
         * Old code:
         * return null;
         *
         * New code:
         * //allows this method to build fluid handlers off of FluidStates
         * return Hooks.getFluidStateHandler(world, blockPos);
         */
        if(index == 1 && insn.getOpcode() == ACONST_NULL) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(genMethodNode("getFluidHandler", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraftforge/fluids/capability/IFluidHandler;"));
            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }
        /*
         * tryPickupFluid: (changes are around line 565)
         * Old code:
         * IBlockState state = worldIn.getBlockState(pos);
         *
         * New code:
         * //check for FluidState before assuming no fluid blocks are at the position
         * IBlockState state = FluidloggedUtils.getFluidOrReal(worldIn, pos);
         */
        else if(index == 2 && checkMethod(insn, obfuscated ? "func_180495_p" : "getBlockState", null)) {
            instructions.insert(insn, genMethodNode("git/jbredwards/fluidlogged_api/api/util/FluidloggedUtils", "getFluidOrReal", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"));
            instructions.remove(insn);
            return true;
        }
        /*
         * tryPlaceFluid: (changes are around line 640)
         * Old code:
         * if (!world.isAirBlock(pos) && !isDestNonSolid && !isDestReplaceable)
         * {
         *     return false;
         * }
         *
         * New code:
         * //if the position can't be fluidlogged and isn't replaceable, return false
         * if (!Hooks.isFluidloggable(world, pos, fluid, destBlockState) && !isDestNonSolid && !isDestReplaceable)
         * {
         *     return false;
         * }
         */
        else if(index == 3 && checkMethod(insn, obfuscated ? "func_175623_d" : "isAirBlock", null)) {
            final InsnList list = new InsnList();
            //Fluid local var
            list.add(new VarInsnNode(ALOAD, 5));
            //IBlockState local var
            list.add(new VarInsnNode(ALOAD, 6));
            //add new code
            list.add(genMethodNode("isFluidloggable", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;Lnet/minecraft/block/state/IBlockState;)Z"));
            instructions.insertBefore(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nullable
        public static IFluidHandler getFluidStateHandler(@Nonnull World world, @Nonnull BlockPos pos) {
            final FluidState fluidState = FluidState.get(world, pos);
            return fluidState.isValid() ? new FluidBlockWrapper(fluidState.getFluidBlock(), world, pos) : null;
        }

        public static boolean isFluidloggable(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull Fluid fluid, @Nonnull IBlockState destBlockState) {
            return FluidloggedUtils.isStateFluidloggable(destBlockState, world, pos, fluid);
        }
    }
}

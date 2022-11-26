package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.Fluid;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.ClassNode;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * fix crash with PlusTweaks mod fluid interactions
 * @author jbred
 *
 */
public final class PluginPlusTweaks implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * doInteraction:
         * New code:
         * //only do interaction if fluids are touching
         * private void doInteraction(IBlockState state, World world, BlockPos pos, Block neighborBlock, BlockPos neighbourPos)
         * {
         *     Hooks.doFluidInteraction(state, world, pos, neighborPos, this.definedFluid, ActionRegisterLiquidInteraction.interactions);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("doInteraction"),
            "doFluidInteraction", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/Fluid;Ljava/util/HashMap;)V", generator -> {
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 5);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "definedFluid", "Lnet/minecraftforge/fluids/Fluid;");
                generator.visitFieldInsn(GETSTATIC, "plus/misterplus/plustweaks/compact/crafttweaker/actions/ActionRegisterLiquidInteraction", "interactions", "Ljava/util/HashMap;");
            }
        );
        /*
         * injectCheckForMixing:
         * New code:
         * //only do interaction if fluids are touching
         * private void injectCheckForMixing(World worldIn, BlockPos pos, IBlockState state, CallbackInfoReturnable<Boolean> cir)
         * {
         *     cir.setReturnValue(Hooks.doLiquidInteraction(worldIn, pos, state, cir.getReturnValueZ(), ActionRegisterLiquidInteraction.interactions));
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("injectCheckForMixing"), null, null, generator -> {
            generator.visitVarInsn(ALOAD, 4);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ALOAD, 3);
            generator.visitVarInsn(ALOAD, 4);
            generator.visitMethodInsn(INVOKEVIRTUAL, "org/spongepowered/asm/mixin/injection/callback/CallbackInfoReturnable", "getReturnValueZ", "()Z", false);
            generator.visitFieldInsn(GETSTATIC, "plus/misterplus/plustweaks/compact/crafttweaker/actions/ActionRegisterLiquidInteraction", "interactions", "Ljava/util/HashMap;");
            generator.visitMethodInsn(INVOKESTATIC, getHookClass(), "doLiquidInteraction", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;ZLjava/util/HashMap;)Ljava/lang/Boolean;", false);
            generator.visitMethodInsn(INVOKEVIRTUAL, "org/spongepowered/asm/mixin/injection/callback/CallbackInfoReturnable", "setReturnValue", "(Ljava/lang/Object;)V", false);
        });

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void doFluidInteraction(@Nonnull IBlockState state, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull BlockPos neighborPos, @Nonnull Fluid definedFluid, @Nonnull HashMap<String, HashMap<Integer, IBlockState>> interactions) {
            if(!pos.equals(neighborPos)) {
                final IBlockState here = world.getBlockState(pos);
                final EnumFacing facing = EnumFacing.getFacingFromVector(
                        neighborPos.getX() - pos.getX(),
                        neighborPos.getY() - pos.getY(),
                        neighborPos.getZ() - pos.getZ());

                if(FluidloggedUtils.canFluidFlow(world, pos, here, facing)) {
                    if(facing == EnumFacing.UP && !definedFluid.isLighterThanAir()) return;
                    if(facing == EnumFacing.DOWN && definedFluid.isLighterThanAir()) return;

                    final IBlockState neighbor = world.getBlockState(neighborPos);
                    if(FluidloggedUtils.canFluidFlow(world, neighborPos, neighbor, facing.getOpposite())) {
                        final FluidState neighborFluid = FluidloggedUtils.getFluidState(world, neighborPos, neighbor);
                        if(!neighborFluid.isEmpty()) {
                            if(facing == EnumFacing.UP && neighborFluid.getFluid().isLighterThanAir()) return;
                            if(facing == EnumFacing.DOWN && !neighborFluid.getFluid().isLighterThanAir()) return;

                            final @Nullable Pair<String, Boolean> interactionKey = findKey(definedFluid, neighborFluid.getFluid(), interactions);
                            if(interactionKey != null) {
                                final HashMap<Integer, IBlockState> blockList = interactions.get(interactionKey.getKey());
                                final BlockPos actualPos = interactionKey.getValue() ? pos : neighborPos;

                                if((interactionKey.getValue() ? here : neighbor).getBlock().isReplaceable(world, actualPos)) {
                                    final Integer level = interactionKey.getValue() ? state.getValue(BlockLiquid.LEVEL) : neighborFluid.getLevel();
                                    if(blockList.containsKey(level)) world.setBlockState(actualPos, ForgeEventFactory
                                            .fireFluidPlaceBlockEvent(world, actualPos, actualPos, blockList.get(level)));
                                }
                            }
                        }
                    }
                }
            }
        }

        @Nonnull
        public static Boolean doLiquidInteraction(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, boolean oldValue, @Nonnull HashMap<String, HashMap<Integer, IBlockState>> interactions) {
            final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromState(state);
            if(fluid != null) {
                final IBlockState here = world.getBlockState(pos);
                final List<Runnable> surrounding = new ArrayList<>();

                for(EnumFacing facing : EnumFacing.values()) {
                    if(facing != EnumFacing.UP && FluidloggedUtils.canFluidFlow(world, pos, here, facing)) {
                        final BlockPos neighborPos = pos.offset(facing);
                        final IBlockState neighbor = world.getBlockState(neighborPos);

                        if(FluidloggedUtils.canFluidFlow(world, neighborPos, neighbor, facing.getOpposite())) {
                            final FluidState neighborFluid = FluidloggedUtils.getFluidState(world, neighborPos, neighbor);
                            if(!neighborFluid.isEmpty() && (facing != EnumFacing.DOWN || neighborFluid.getFluid().isLighterThanAir())) {
                                final @Nullable Pair<String, Boolean> interactionKey = findKey(fluid, neighborFluid.getFluid(), interactions);
                                if(interactionKey != null) {
                                    final HashMap<Integer, IBlockState> blockList = interactions.get(interactionKey.getKey());
                                    final BlockPos actualPos = interactionKey.getValue() ? pos : neighborPos;

                                    if((interactionKey.getValue() ? here : neighbor).getBlock().isReplaceable(world, actualPos)) {
                                        final Integer level = interactionKey.getValue() ? state.getValue(BlockLiquid.LEVEL) : neighborFluid.getLevel();
                                        if(blockList.containsKey(level)) {
                                            final IBlockState newState = ForgeEventFactory.fireFluidPlaceBlockEvent(world, actualPos, actualPos, blockList.get(level));
                                            if(interactionKey.getValue()) {
                                                world.setBlockState(pos, newState);
                                                return Boolean.TRUE;
                                            }

                                            else surrounding.add(() -> world.setBlockState(neighborPos, newState));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if(!surrounding.isEmpty()) {
                    surrounding.forEach(Runnable::run);
                    return Boolean.TRUE;
                }
            }

            return oldValue;
        }

        //helper
        @Nullable
        private static Pair<String, Boolean> findKey(@Nonnull Fluid fluid1, @Nonnull Fluid fluid2, @Nonnull HashMap<String, HashMap<Integer, IBlockState>> interactions) {
            final ResourceLocation loc1 = fluid1.getBlock().getRegistryName();
            final ResourceLocation loc2 = fluid2.getBlock().getRegistryName();

            final String key1 = loc1 + ":" + loc2;
            if(interactions.containsKey(key1)) return Pair.of(key1, true);

            final String key2 = loc2 + ":" + loc1;
            return interactions.containsKey(key2) ? Pair.of(key2, false) : null;
        }
    }
}

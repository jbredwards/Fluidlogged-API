package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.gen.structure.template.ITemplateProcessor;
import net.minecraft.world.gen.structure.template.PlacementSettings;
import net.minecraft.world.gen.structure.template.Template;
import org.apache.commons.lang3.tuple.Pair;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

/**
 * structures can load saved FluidStates
 * @author jbred
 *
 */
@Mixin(Template.class)
public abstract class MixinTemplate
{
    @Shadow
    private BlockPos size;
    private FluidState entry;
    private final List<Pair<BlockPos, FluidState>> fluidStates = new ArrayList<>();

    @Inject(method = "takeBlocksFromWorld", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/BlockPos;add(Lnet/minecraft/util/math/Vec3i;)Lnet/minecraft/util/math/BlockPos;"))
    private void clearOldFluidStates(@Nonnull World worldIn, @Nonnull BlockPos startPos, @Nonnull BlockPos endPos, boolean takeEntities, @Nullable Block toIgnore, @Nonnull CallbackInfo ci) {
        fluidStates.clear();
    }

    @Nonnull
    @Redirect(method = "takeBlocksFromWorld", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    private IBlockState takeFluidStatesFromWorld(@Nonnull World world, @Nonnull BlockPos pos) {
        //save FluidState for later use
        entry = FluidState.get(world, pos);
        //still gets the block state
        return world.getBlockState(pos);
    }

    @Redirect(method = "takeBlocksFromWorld", at = @At(value = "INVOKE", target = "Ljava/util/List;add(Ljava/lang/Object;)Z", remap = false))
    private boolean addFluidStates(List<Object> blocks, Object info) {
        if(!entry.isEmpty()) fluidStates.add(Pair.of(((Template.BlockInfo)info).pos, entry));
        return blocks.add(info);
    }

    @Redirect(
            method = "addBlocksToWorld(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/gen/structure/template/ITemplateProcessor;Lnet/minecraft/world/gen/structure/template/PlacementSettings;I)V",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;setBlockState(Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"))
    private boolean setBlockState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, int blockFlags) {
        return world.setBlockState(pos, state, blockFlags | 32);
    }

    @Inject(
            method = "addBlocksToWorld(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/gen/structure/template/ITemplateProcessor;Lnet/minecraft/world/gen/structure/template/PlacementSettings;I)V",
            at = @At("RETURN"))
    private void addFluidsToWorld(@Nonnull World worldIn, @Nonnull BlockPos pos, @Nullable ITemplateProcessor templateProcessor, @Nonnull PlacementSettings placementIn, int flags, @Nonnull CallbackInfo ci) {
        if(!fluidStates.isEmpty() && size.getX() > 0 && size.getY() > 0 && size.getZ() > 0) {
            for(Pair<BlockPos, FluidState> entry : fluidStates) {
                BlockPos transformedPos = Template.transformedBlockPos(placementIn, entry.getKey()).add(pos);
                FluidloggedUtils.setFluidState(worldIn, transformedPos, null, entry.getValue(), false, flags);
            }
        }
    }

    @Inject(method = "writeToNBT", at = @At(value = "INVOKE", target = "Lnet/minecraft/nbt/NBTTagList;<init>()V", ordinal = 0))
    private void writeFluidStates(@Nonnull NBTTagCompound compound, @Nonnull CallbackInfoReturnable<NBTTagCompound> cir) {
         if(!fluidStates.isEmpty()) {
            final NBTTagList list = new NBTTagList();
            for(Pair<BlockPos, FluidState> entry : fluidStates) {
                NBTTagCompound nbt = new NBTTagCompound();
                nbt.setString("state", String.valueOf(entry.getValue().getBlock().getRegistryName()));
                nbt.setLong("pos", entry.getKey().toLong());

                list.appendTag(nbt);
            }

            compound.setTag("fluidStates", list);
        }
    }

    @Inject(method = "read", at = @At(value = "INVOKE", target = "Ljava/util/List;clear()V", remap = false, ordinal = 0))
    private void readFluidStates(@Nonnull NBTTagCompound compound, @Nonnull CallbackInfo ci) {
        fluidStates.clear();

        if(compound.hasKey("fluidStates")) {
            for(NBTBase nbtBase : compound.getTagList("fluidStates", 10)) {
                NBTTagCompound nbt = (NBTTagCompound)nbtBase;
                FluidState fluidState = FluidState.of(Block.getBlockFromName(nbt.getString("state")));

                if(!fluidState.isEmpty())
                    fluidStates.add(Pair.of(BlockPos.fromLong(nbt.getLong("pos")), fluidState));
            }
        }
    }
}

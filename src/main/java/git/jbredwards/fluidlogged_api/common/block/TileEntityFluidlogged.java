package git.jbredwards.fluidlogged_api.common.block;

import git.jbredwards.fluidlogged_api.Fluidlogged;
import git.jbredwards.fluidlogged_api.common.capability.IFluidloggedCapability;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;
import java.util.Optional;

/**
 * used when a BlockFluidloggedTE is storing a block with no tile entity
 * @author jbred
 *
 */
@ParametersAreNonnullByDefault
public final class TileEntityFluidlogged extends TileEntity
{
    //client packet type (non-final just in case other mods have to change this)
    public static int packetType = 2;

    //preserve worlds that used old versions of this mod
    @SuppressWarnings("deprecation")
    @Override
    public void readFromNBT(NBTTagCompound compound) {
        if(compound.hasKey("Stored", Constants.NBT.TAG_COMPOUND)) {
            final NBTTagCompound nbt = compound.getCompoundTag("Stored");
            if(nbt.hasKey("id", Constants.NBT.TAG_STRING) && nbt.hasKey("meta", Constants.NBT.TAG_INT)) {
                final @Nullable IFluidloggedCapability cap = Fluidlogged.CAPABILITY.get(this);
                //restore old stored value
                if(cap != null) cap.setStored(Optional.ofNullable(Block.getBlockFromName(nbt.getString("id"))).orElse(Blocks.BARRIER).getStateFromMeta(nbt.getInteger("meta")));
            }
        }

        super.readFromNBT(compound);
    }

    @Override
    public boolean shouldRefresh(World world, BlockPos pos, IBlockState oldState, IBlockState newState) {
        return oldState.getBlock() != newState.getBlock();
    }
}

package git.jbredwards.fluidlogged.common.block;

import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.NetworkManager;
import net.minecraft.network.play.server.SPacketUpdateTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ITickable;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nonnull;
import java.util.Optional;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("NullableProblems")
public class TileEntityFluidlogged extends TileEntity implements ITickable
{
    //the fluidlogged block (default barrier so nonnull)
    @Nonnull protected IBlockState stored = Blocks.BARRIER.getDefaultState();
    //any tags unique to this fluidlogged block
    @Nonnull public NBTTagCompound storedNBT = new NBTTagCompound();
    //how many ticks this has existed, note that this isn't saved via nbt
    public int ticksExisted = 0;

    public IBlockState getStored() { return stored; }

    //should only be used when called on both client & server
    public void setStoredSoft(@Nonnull IBlockState storedIn) {
        stored = storedIn;
    }

    public void setStored(@Nonnull IBlockState storedIn, boolean notify) {
        stored = storedIn;
        markDirty();

        if(world != null && pos != null) {
            final IBlockState here = world.getBlockState(pos);
            world.addBlockEvent(pos, here.getBlock(), 1, Block.getStateId(stored));

            if(notify) {
                world.markAndNotifyBlock(pos, world.getChunkFromBlockCoords(pos), here, stored, 3);
            }
        }
    }

    @Override
    public NBTTagCompound writeToNBT(NBTTagCompound compound) {
        final NBTTagCompound nbt = new NBTTagCompound();
        nbt.setString("id", Optional.ofNullable(stored.getBlock().getRegistryName()).orElse(new ResourceLocation("barrier")).toString());
        nbt.setInteger("meta", stored.getBlock().getMetaFromState(stored));
        nbt.setTag("tags", storedNBT);

        compound.setTag("Stored", nbt);
        return super.writeToNBT(compound);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void readFromNBT(NBTTagCompound compound) {
        if(compound.hasKey("Stored", Constants.NBT.TAG_COMPOUND)) {
            final NBTTagCompound nbt = compound.getCompoundTag("Stored");
            stored = Optional.ofNullable(Block.getBlockFromName(nbt.getString("id"))).orElse(Blocks.BARRIER).getStateFromMeta(nbt.getInteger("meta"));
            storedNBT = nbt.getCompoundTag("tags");
        }

        super.readFromNBT(compound);
    }

    @Override
    public NBTTagCompound getUpdateTag() {
        return writeToNBT(new NBTTagCompound());
    }

    @Override
    public SPacketUpdateTileEntity getUpdatePacket() {
        return new SPacketUpdateTileEntity(pos, 1, getUpdateTag());
    }

    @Override
    public void onDataPacket(NetworkManager net, SPacketUpdateTileEntity pkt) {
        readFromNBT(pkt.getNbtCompound());
    }

    @Override
    public boolean receiveClientEvent(int id, int type) {
        if(id == 1) stored = Block.getStateById(type);
        return id == 1;
    }

    @Override
    public boolean shouldRefresh(World world, BlockPos pos, IBlockState oldState, IBlockState newState) {
        if(!(oldState.getBlock() instanceof BlockFluidloggedTE)) return false;
        return oldState.getBlock() != newState.getBlock();
    }

    @Override
    public void update() {
        if(hasWorld() && stored.getBlock() instanceof IFluidloggable) {
            ++ticksExisted;
            ((IFluidloggable)stored.getBlock()).fluidloggedTick(this);
        }
    }

    @Override
    public boolean shouldRenderInPass(int pass) {
        return super.shouldRenderInPass(pass);
    }
}

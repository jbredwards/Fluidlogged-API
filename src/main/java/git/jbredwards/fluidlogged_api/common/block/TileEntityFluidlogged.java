package git.jbredwards.fluidlogged_api.common.block;

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
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
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
    //should only be changed through setStored if possible
    @Nonnull public IBlockState stored = Blocks.BARRIER.getDefaultState();
    //any tags unique to this fluidlogged block
    @Nonnull public NBTTagCompound storedNBT = new NBTTagCompound();
    //optional cache for anything stored in the 'storedNBT', as to not require you to have to get from the nbt every time
    //note that this cache is not saved via nbt by default, for that implement IFluidloggable
    @Nullable public Object[] cache;
    //how many ticks this has existed
    public int ticksExisted = 0;

    public void setStored(@Nonnull IBlockState storedIn, boolean notify) {
        stored = storedIn;
        markDirty();

        if(world != null && pos != null) {
            final IBlockState here = world.getBlockState(pos);
            world.addBlockEvent(pos, here.getBlock(), 1, Block.getStateId(stored));

            if(notify) {
                world.scheduleUpdate(pos, blockType, blockType.tickRate(world));
                world.markAndNotifyBlock(pos, world.getChunkFromBlockCoords(pos), here, here, 3);
            }
        }
    }

    @Override
    public NBTTagCompound writeToNBT(NBTTagCompound compound) {
        //save optional cache to storedNBT or do other things
        if(stored.getBlock() instanceof IFluidloggable) ((IFluidloggable)stored.getBlock()).writeToStoredNBT(this);

        //saves built-in data
        final NBTTagCompound nbt = new NBTTagCompound();
        nbt.setString("id", Optional.ofNullable(stored.getBlock().getRegistryName()).orElse(new ResourceLocation("barrier")).toString());
        nbt.setInteger("meta", stored.getBlock().getMetaFromState(stored));
        nbt.setTag("tags", storedNBT);
        nbt.setInteger("ticksExisted", ticksExisted);

        compound.setTag("Stored", nbt);

        return super.writeToNBT(compound);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void readFromNBT(NBTTagCompound compound) {
        //saves built-in data
        if(compound.hasKey("Stored", Constants.NBT.TAG_COMPOUND)) {
            final NBTTagCompound nbt = compound.getCompoundTag("Stored");
            stored = Optional.ofNullable(Block.getBlockFromName(nbt.getString("id"))).orElse(Blocks.BARRIER).getStateFromMeta(nbt.getInteger("meta"));
            storedNBT = nbt.getCompoundTag("tags");
            ticksExisted = nbt.getInteger("ticksExisted");
        }

        super.readFromNBT(compound);

        //restore optional cache or do other things
        if(stored.getBlock() instanceof IFluidloggable) ((IFluidloggable)stored.getBlock()).readFromStoredNBT(this);
    }

    //gets the value from the given index
    //returns the cached value, or the fallback if there's no value
    @SuppressWarnings("unchecked")
    @Nullable
    public <V> V getValue(int index, @Nullable V fallback) {
        if(cache == null || cache.length <= index) setValue(index, fallback);
        //returns the cached value
        try { return (V)cache[index]; }
        //cached value is a different type, this shouldn't happen
        catch(Exception e) { return fallback; }
    }

    //sets the value at the given index
    public void setValue(int index, @Nullable Object value) {
        //generates a new empty cache if the old one doesn't exist
        if(cache == null) cache = new Object[index + 1];
        //generates a new cache with all of the old one's values
        if(cache.length <= index) {
            final Object[] newCache = new Object[index + 1];
            for(int i = 0; i <= index; i++) {
                if(i < cache.length) newCache[i] = cache[i];
            }
            cache = newCache;
        }
        //sets the new value
        cache[index] = value;
    }

    @Override
    public void update() {
        if(hasWorld() && pos != null) {
            ticksExisted++;
            if(stored.getBlock() instanceof IFluidloggable) ((IFluidloggable)stored.getBlock()).fluidloggedTick(this);
        }
    }

    @SideOnly(Side.CLIENT)
    @Override
    public double getMaxRenderDistanceSquared() {
        return stored.getBlock() instanceof IFluidloggable && ((IFluidloggable)stored.getBlock()).doFluidloggedTESR(this) ? 4096 : -1;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public boolean canRenderBreaking() {
        return stored.hasCustomBreakingProgress();
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
        if(world != null && pos != null) {
            world.checkLightFor(EnumSkyBlock.BLOCK, pos);
            world.markBlockRangeForRenderUpdate(pos, pos);
        }
    }

    @Override
    public boolean receiveClientEvent(int id, int type) {
        if(id == 1) stored = Block.getStateById(type);
        return id == 1;
    }

    @Override
    public boolean shouldRefresh(World world, BlockPos pos, IBlockState oldState, IBlockState newState) {
        return oldState.getBlock() != newState.getBlock();
    }
}

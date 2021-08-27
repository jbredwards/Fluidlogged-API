package git.jbredwards.fluidlogged_api.common.capability;

import git.jbredwards.fluidlogged_api.common.block.TileEntityFluidlogged;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.Constants;

import javax.annotation.Nullable;
import javax.annotation.ParametersAreNonnullByDefault;
import java.util.Optional;

/**
 * allows vanilla tile entities and supported modded ones to be fluidlogged without a massive rewrite
 * @author jbred
 *
 */
@ParametersAreNonnullByDefault
public interface IFluidloggedCapability extends Capability.IStorage<IFluidloggedCapability>
{
    //get value
    @Nullable
    IBlockState getStored();
    //gets the previous stored value (used only for ChunkPlugin)
    @Nullable
    IBlockState getPreviousStored();
    //set the stored value, but for the client preferably
    void setStored(@Nullable IBlockState storedIn);
    //set value (only call this serverside please)
    //world & pos can be null if you don't want to sync the client for some reason
    void setStored(@Nullable World world, @Nullable BlockPos pos, @Nullable IBlockState storedIn, boolean notify);
    //get capability
    Capability<IFluidloggedCapability> getCapability();
    //get this from an entity
    @Nullable
    default IFluidloggedCapability get(@Nullable ICapabilityProvider p) {
        return p != null && p.hasCapability(getCapability(), null) ? p.getCapability(getCapability(), null) : null;
    }

    //default implementation
    class Impl implements IFluidloggedCapability
    {
        @CapabilityInject(IFluidloggedCapability.class)
        public static final Capability<IFluidloggedCapability> CAPABILITY = null;
        //stored value (if null, the tile entity isn't fluidlogged)
        @Nullable protected IBlockState stored;
        //previous stored value (used only for ChunkPlugin)
        @Nullable protected IBlockState prevStored;

        @Nullable
        @Override
        public IBlockState getStored() { return stored; }

        @Nullable
        @Override
        public IBlockState getPreviousStored() { return prevStored; }

        @Override
        public void setStored(@Nullable IBlockState storedIn) {
            prevStored = stored;
            stored = storedIn;
        }

        @Override
        public void setStored(@Nullable World world, @Nullable BlockPos pos, @Nullable IBlockState storedIn, boolean notify) {
            if(!equalsNullable(storedIn)) {
                setStored(storedIn);
                //notify client of change
                if(world != null && pos != null) {
                    world.addBlockEvent(pos, world.getBlockState(pos).getBlock(), TileEntityFluidlogged.packetType, stored == null ? 0 : Block.getStateId(stored));
                    if(notify) {
                        final IBlockState here = world.getBlockState(pos);
                        world.scheduleUpdate(pos, here.getBlock(), here.getBlock().tickRate(world));
                        world.markAndNotifyBlock(pos, world.getChunkFromBlockCoords(pos), here, here, 3);
                    }
                }
            }
        }

        //Object::equals, but nullable both ways
        protected boolean equalsNullable(@Nullable IBlockState storedIn) {
            if(stored == null && storedIn == null) return true;
            else if(stored == null || storedIn == null) return false;
            else return stored.equals(storedIn);
        }

        @Override
        public Capability<IFluidloggedCapability> getCapability() { return CAPABILITY; }

        @Override
        public NBTBase writeNBT(Capability<IFluidloggedCapability> capability, IFluidloggedCapability instance, EnumFacing side) {
            final Impl impl = (Impl)instance;
            //not fluidlogged
            if(impl.stored == null) return new NBTTagCompound();
            //if stored state exists
            final NBTTagCompound nbt = new NBTTagCompound();
            nbt.setString("id", Optional.ofNullable(impl.stored.getBlock().getRegistryName()).orElse(new ResourceLocation("barrier")).toString());
            nbt.setInteger("meta", impl.stored.getBlock().getMetaFromState(impl.stored));
            return nbt;
        }

        @SuppressWarnings("deprecation")
        @Override
        public void readNBT(Capability<IFluidloggedCapability> capability, IFluidloggedCapability instance, EnumFacing side, NBTBase nbtIn) {
            //if stored state exists
            if(nbtIn instanceof NBTTagCompound) {
                final NBTTagCompound nbt = (NBTTagCompound)nbtIn;
                if(nbt.hasKey("id", Constants.NBT.TAG_STRING) && nbt.hasKey("meta", Constants.NBT.TAG_INT)) {
                    ((Impl)instance).stored = Optional.ofNullable(Block.getBlockFromName(nbt.getString("id"))).orElse(Blocks.BARRIER).getStateFromMeta(nbt.getInteger("meta"));
                }
            }
        }
    }
}

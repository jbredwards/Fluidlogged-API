package git.jbredwards.fluidlogged_api.common.storage;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import git.jbredwards.fluidlogged_api.common.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.ITileEntityProvider;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ITickable;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Objects;

/**
 * allows worlds with legacy versions of this mod to load with newer ones
 * @author jbred
 *
 */
public final class OldWorldFixer extends Block implements ITileEntityProvider
{
    final Fluid fluid;

    public OldWorldFixer(@Nonnull Fluid fluid) {
        super(fluid.getBlock().getDefaultState().getMaterial());
        this.fluid = fluid;

        setRegistryName(new ResourceLocation(fluid.getName()).getResourcePath() + "logged_te");
    }

    @Nonnull
    @Override
    public TileEntity createNewTileEntity(@Nullable World worldIn, int meta) { return new Tile(); }

    //first tick this is loaded, convert to a FluidState
    public static final class Tile extends TileEntity implements ITickable
    {
        IBlockState stored;

        @Override
        public void update() {
            if(stored != null && !world.isRemote && world.isBlockLoaded(pos)) {
                final FluidState fluidState = FluidState.of(((OldWorldFixer)getBlockType()).fluid);
                world.setBlockState(pos, stored);
                FluidloggedUtils.setFluidState(world, pos, stored, fluidState, false);
            }
        }

        @SuppressWarnings("deprecation")
        @Override
        public void readFromNBT(@Nonnull NBTTagCompound compound) {
            if(compound.hasKey("Stored", Constants.NBT.TAG_COMPOUND)) {
                final NBTTagCompound nbt = compound.getCompoundTag("Stored");
                stored = Objects.requireNonNull(Block.getBlockFromName(nbt.getString("id"))).getStateFromMeta(nbt.getInteger("meta"));
            }

            super.readFromNBT(compound);
        }
    }
}

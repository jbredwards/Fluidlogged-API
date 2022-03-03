package git.jbredwards.fluidlogged_api.asm.mixins.vanilla.block.fluidloggable;

import git.jbredwards.fluidlogged_api.common.block.IFluidloggable;
import net.minecraft.block.Block;
import net.minecraft.block.BlockBarrier;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.EnumParticleTypes;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.spongepowered.asm.mixin.Mixin;

import javax.annotation.Nonnull;
import java.util.Random;

/**
 * makes barriers fluidloggable by default
 * @author jbred
 *
 */
@Mixin(BlockBarrier.class)
public abstract class BlockBarrierMixin extends Block implements IFluidloggable
{
    public BlockBarrierMixin(@Nonnull Material materialIn) { super(materialIn); }

    //moved the hardcoded stuff from WorldClient to here
    @SideOnly(Side.CLIENT)
    @Override
    public void randomDisplayTick(@Nonnull IBlockState stateIn, @Nonnull World worldIn, @Nonnull BlockPos pos, @Nonnull Random rand) {
        final EntityPlayer player = Minecraft.getMinecraft().player;
        if(player.isCreative() && player.getHeldItemMainhand().getItem() == getItemDropped(stateIn, worldIn.rand, 0)) {
            worldIn.spawnParticle(EnumParticleTypes.BARRIER, pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, 0, 0, 0);
        }
    }
}

package git.jbredwards.fluidlogged.client.util;

import git.jbredwards.fluidlogged.common.block.IFluidloggable;
import git.jbredwards.fluidlogged.common.block.TileEntityFluidlogged;
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

/**
 *
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class FluidloggedTESR extends TileEntitySpecialRenderer<TileEntityFluidlogged>
{
    @Override
    public void render(TileEntityFluidlogged te, double x, double y, double z, float partialTicks, int destroyStage, float alpha) {
        if(te.hasWorld() && te.getStored().getBlock() instanceof IFluidloggable) {
            ((IFluidloggable)te.getStored().getBlock()).fluidloggedRenderTick(te, x, y, z, partialTicks, destroyStage, alpha);
        }
    }
}

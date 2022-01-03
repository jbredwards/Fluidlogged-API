package git.jbredwards.fluidlogged_api.common.hwyla;

import git.jbredwards.fluidlogged_api.common.util.FluidState;
import mcp.mobius.waila.api.*;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidActionResult;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.IFluidHandler;

import javax.annotation.Nonnull;
import java.awt.*;
import java.util.List;

/**
 * WIP
 * @author jbred
 *
 */
public final class HwylaPlugin implements IWailaPlugin
{
    @Override
    public void register(IWailaRegistrar registrar) {

    }

    public enum Provider implements IWailaDataProvider
    {
        INSTANCE;

        @Nonnull
        @Override
        public List<String> getWailaHead(ItemStack itemStack, List<String> tooltip, IWailaDataAccessor accessor, IWailaConfigHandler config) {
            final FluidState fluidState = FluidState.get(accessor.getWorld(), accessor.getPosition());
            //default
            return tooltip;
        }
    }

    public enum Renderer implements IWailaTooltipRenderer
    {
        INSTANCE;

        @Nonnull
        @Override
        public Dimension getSize(String[] params, IWailaCommonAccessor accessor) {
            return null;
        }

        @Override
        public void draw(String[] params, IWailaCommonAccessor accessor) {

        }

        //copied from mcp.mobius.waila.addons.core.HUDHandlerFluids
        @Nonnull
        private static ItemStack getStackFromLiquid(IWailaDataAccessor accessor) {
            IFluidHandler fluidHandler = FluidUtil.getFluidHandler(accessor.getWorld(), accessor.getPosition(), accessor.getSide());
            if (fluidHandler == null)
                return ItemStack.EMPTY;

            FluidStack stack = fluidHandler.drain(Fluid.BUCKET_VOLUME, false);
            if (stack == null)
                return ItemStack.EMPTY;

            ItemStack bucket = FluidUtil.getFilledBucket(stack);
            if (!bucket.isEmpty())
                return bucket;

            FluidActionResult result = FluidUtil.tryFillContainer(new ItemStack(Items.BUCKET), fluidHandler, 1, null, false);
            return result.isSuccess() ? result.getResult() : ItemStack.EMPTY;
        }
    }
}

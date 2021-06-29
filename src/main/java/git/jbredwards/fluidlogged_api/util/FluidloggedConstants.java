package git.jbredwards.fluidlogged_api.util;

import git.jbredwards.fluidlogged_api.common.block.BlockFluidloggedTE;
import net.minecraftforge.fluids.Fluid;

import javax.annotation.Nonnull;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jbred
 *
 */
public enum FluidloggedConstants
{
    ;

    @Nonnull public static final String MODID = "fluidlogged_api";
    @Nonnull public static final String NAME = "Fluidlogged API";
    @Nonnull public static final String VERSION = "1.4.3";

    //used to get the fluidlogged te's from the fluid
    @Nonnull public static Map<Fluid, BlockFluidloggedTE> FLUIDLOGGED_TE_LOOKUP = new HashMap<>();
}

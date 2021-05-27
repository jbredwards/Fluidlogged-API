package git.jbredwards.fluidlogged.util;

import git.jbredwards.fluidlogged.common.block.BlockFluidloggedTE;
import net.minecraftforge.fluids.Fluid;

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

    public static final String MODID = "fluidlogged_api";
    public static final String NAME = "Fluidlogged API";
    public static final String VERSION = "1.3.2";

    //used to get the fluidlogged te's from the fluid
    public static final Map<Fluid, BlockFluidloggedTE> FLUIDLOGGED_TE_LOOKUP = new HashMap<>();
}

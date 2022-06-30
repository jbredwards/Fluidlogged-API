package git.jbredwards.fluidlogged_api.mod.common.config.js;

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.Immutable;
import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Objects;

/**
 *
 * @author jbred
 *
 */
@Immutable
public final class JSFluid
{
    @Nonnull final FluidState fluidState;
    public JSFluid(@Nonnull FluidState fluidStateIn) { fluidState = fluidStateIn; }

    @Nonnull
    public static JSFluid of(@Nonnull String name) {
        final @Nullable Fluid fluid = FluidloggedUtils.getFluidFromBlock(Block.getBlockFromName(name));
        if(fluid != null) return new JSFluid(FluidState.of(fluid));
        else return new JSFluid(FluidState.of(FluidRegistry.getFluid(name)));
    }

    @Nullable
    public Fluid getJavaFluid() { return fluidState.getFluid(); }

    public boolean isEmpty() { return fluidState.isEmpty(); }

    @SuppressWarnings("rawtypes")
    public boolean hasTag(@Nullable Object o) {
        if(fluidState.isEmpty() || o == null) return false;
        else if(equals(o)) return true;

        //check for any Fluid instance
        else if(o instanceof Fluid) return fluidState.getFluid() == o
                || FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), (Fluid)o);

        //check for any String instance
        else if(o instanceof String)
            return fluidState.getFluid().getName().equals(o)
                    //check the fluid block name
                    || fluidState.getBlock().getRegistryName() != null
                    && fluidState.getBlock().getRegistryName().toString().equals(o);

        //check for any Array instance
        else if(o.getClass().isArray()) {
            final int length = Array.getLength(o);
            for(int i = 0; i < length; i++) if(hasTag(Array.get(o, i))) return true;
        }

        //check for any Collection instance
        else if(o instanceof Collection) return ((Collection)o).contains(fluidState.getFluid().getName())
                || ((Collection)o).contains(Objects.requireNonNull(fluidState.getBlock().getRegistryName()).toString());

        //check for any Iterable instance
        else if(o instanceof Iterable) for(@Nullable Object obj : (Iterable)o) if(hasTag(obj)) return true;

        return false;
    }

    @Override
    public boolean equals(@Nullable Object o) {
        if(!(o instanceof JSFluid)) return false;
        return o == this || ((JSFluid)o).fluidState.equals(fluidState)
                || FluidloggedUtils.isCompatibleFluid(fluidState.getFluid(), ((JSFluid)o).fluidState.getFluid());
    }

    @Nonnull
    public JSSide getFacingDir() { return getDensityDir() > 0 ? JSSide.DOWN : JSSide.UP; }

    public int getDensityDir() { return isEmpty() || fluidState.getFluid().getDensity() > 0 ? -1 : 1; }

    public int getLevel() { return fluidState.getLevel(); }
}

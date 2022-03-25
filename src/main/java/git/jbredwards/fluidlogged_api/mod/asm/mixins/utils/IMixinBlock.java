package git.jbredwards.fluidlogged_api.mod.asm.mixins.utils;

import javax.annotation.Nullable;

/**
 * allows the user to override canFluidFlow interactions
 * @author jbred
 *
 */
public interface IMixinBlock
{
    @Nullable Boolean getCanFluidFlow();
    void setCanFluidFlow(@Nullable Boolean canFluidFlowIn);
}

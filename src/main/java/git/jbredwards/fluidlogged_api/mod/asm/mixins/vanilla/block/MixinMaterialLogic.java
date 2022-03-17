package git.jbredwards.fluidlogged_api.mod.asm.mixins.vanilla.block;

import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLogic;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import javax.annotation.Nonnull;

/**
 * prevents fluids from destroying "circuit" blocks
 * @author jbred
 *
 */
@Mixin(MaterialLogic.class)
public abstract class MixinMaterialLogic
{
    @SuppressWarnings("ConstantConditions")
    @Inject(method = "blocksMovement", at = @At("RETURN"), cancellable = true)
    private void blocksMovement(@Nonnull CallbackInfoReturnable<Boolean> cir) {
        cir.setReturnValue((Object)this == Material.CIRCUITS);
    }
}

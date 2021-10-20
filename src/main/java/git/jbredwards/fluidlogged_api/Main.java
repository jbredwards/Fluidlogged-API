package git.jbredwards.fluidlogged_api;

import git.jbredwards.fluidlogged_api.api.capability.IFluidCapability;
import net.minecraft.block.BlockDispenser;
import net.minecraft.init.Items;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

import static git.jbredwards.fluidlogged_api.Constants.*;

/**
 *
 * @author jbred
 *
 */
@SuppressWarnings("unused")
@Mod(modid = MODID, name = NAME, version = VERSION)
public final class Main
{
    //register this mod's capability & packet
    @Mod.EventHandler
    public static void preInit(FMLPreInitializationEvent event) {
        CapabilityManager.INSTANCE.register(IFluidCapability.class, new IFluidCapability.Impl(), IFluidCapability.Impl::new);
    }

    //fixes the vanilla bucket dispenser action by using the forge one instead
    @Mod.EventHandler
    public static void init(FMLInitializationEvent event) {
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.WATER_BUCKET, DispenseFluidContainer.getInstance());
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.LAVA_BUCKET,  DispenseFluidContainer.getInstance());
    }
}

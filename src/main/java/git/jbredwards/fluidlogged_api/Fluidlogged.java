package git.jbredwards.fluidlogged_api;

import git.jbredwards.fluidlogged_api.common.block.BlockFluidloggedTE;
import git.jbredwards.fluidlogged_api.common.capability.IFluidloggedCapability;
import net.minecraft.block.BlockDispenser;
import net.minecraft.block.material.Material;
import net.minecraft.init.Items;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.fluids.DispenseFluidContainer;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

import javax.annotation.Nonnull;

import static git.jbredwards.fluidlogged_api.util.FluidloggedConstants.*;

/**
 *
 * @author jbred
 *
 */
@Mod(modid = MODID, name = NAME, version = VERSION)
public final class Fluidlogged
{
    //vanilla fluidlogged te's, the modded ones get registered automatically through FluidPlugin
    @Nonnull public static BlockFluidloggedTE WATERLOGGED_TE = new BlockFluidloggedTE(FluidRegistry.WATER, Material.WATER);
    @Nonnull public static BlockFluidloggedTE LAVALOGGED_TE = new BlockFluidloggedTE(FluidRegistry.LAVA, Material.LAVA);
    @Nonnull public static IFluidloggedCapability CAPABILITY = new IFluidloggedCapability.Impl();

    //puts the vanilla fluidlogged te's into the lookup
    static {
        FLUIDLOGGED_TE_LOOKUP.put(FluidRegistry.WATER, WATERLOGGED_TE);
        FLUIDLOGGED_TE_LOOKUP.put(FluidRegistry.LAVA, LAVALOGGED_TE);
    }

    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void onPreInit(FMLPreInitializationEvent event) {
        //register capability
        CapabilityManager.INSTANCE.register(IFluidloggedCapability.class, CAPABILITY, () -> CAPABILITY);
    }

    @SuppressWarnings("unused")
    @Mod.EventHandler
    public static void onInit(FMLInitializationEvent event) {
        //fixes the vanilla bucket dispenser action
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.WATER_BUCKET, DispenseFluidContainer.getInstance());
        BlockDispenser.DISPENSE_BEHAVIOR_REGISTRY.putObject(Items.LAVA_BUCKET, DispenseFluidContainer.getInstance());
    }
}

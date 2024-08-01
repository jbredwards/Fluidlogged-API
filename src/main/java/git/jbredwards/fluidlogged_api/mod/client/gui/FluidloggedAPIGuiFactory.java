/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.client.gui;

import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfig;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.resources.I18n;
import net.minecraftforge.common.config.ConfigElement;
import net.minecraftforge.fml.client.IModGuiFactory;
import net.minecraftforge.fml.client.config.DummyConfigElement;
import net.minecraftforge.fml.client.config.IConfigElement;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 *
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public final class FluidloggedAPIGuiFactory implements IModGuiFactory
{
    @Override
    public void initialize(@Nonnull final Minecraft minecraftInstance) {}

    @Override
    public boolean hasConfigGui() { return true; }

    @Nonnull
    @Override
    public GuiScreen createConfigGui(@Nonnull final GuiScreen parentScreen) {
        @Nonnull final List<IConfigElement> configElements = new ArrayList<>();
        configElements.add(ConfigElement.from(FluidloggedAPIConfig.class));
        // special category handlers
        configElements.add(new DummyConfigElement.DummyCategoryElement("fluidlogged_api/fluidTags", "configgui.fluidloggedAPI.fluidTags", Collections.emptyList()));
        configElements.add(new DummyConfigElement.DummyCategoryElement("fluidlogged_api/blacklist", "configgui.fluidloggedAPI.blacklist", Collections.emptyList()));
        configElements.add(new DummyConfigElement.DummyCategoryElement("fluidlogged_api/whitelist", "configgui.fluidloggedAPI.whitelist", Collections.emptyList()));
        // don't change main gui screen
        return new GuiComponentConfig(parentScreen, configElements, FluidloggedAPI.MODID, false, false, I18n.format("configgui.fluidloggedAPI.configTitle"));
    }

    // NO-OP
    @Nullable
    @Override
    public Set<RuntimeOptionCategoryElement> runtimeGuiCategories() { return null; }
}

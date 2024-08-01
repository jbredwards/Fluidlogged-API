/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.client.gui;

import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.fml.client.config.GuiConfig;
import net.minecraftforge.fml.client.config.GuiConfigEntries;
import net.minecraftforge.fml.client.config.IConfigElement;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;

/**
 *
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class GuiComponentConfig extends GuiConfig
{
    public GuiComponentConfig(@Nonnull final GuiScreen parentScreen, @Nonnull final List<IConfigElement> configElements, @Nonnull final String modID, final boolean allRequireWorldRestart, final boolean allRequireMcRestart, @Nonnull final String title) {
        this(parentScreen, configElements, modID, allRequireWorldRestart, allRequireMcRestart, title, null);
    }

    public GuiComponentConfig(@Nonnull final GuiScreen parentScreen, @Nonnull final List<IConfigElement> configElements, @Nonnull final String modID, final boolean allRequireWorldRestart, final boolean allRequireMcRestart, @Nonnull final String title, @Nullable final String titleLine2) {
        super(parentScreen, configElements, modID, allRequireWorldRestart, allRequireMcRestart, title, titleLine2);
    }

    @Override
    public void initGui() {
        final boolean loadComponentEntries = needsRefresh;
        super.initGui();

        if(loadComponentEntries) entryList.listEntries.replaceAll(entry -> entry instanceof GuiConfigEntries.CategoryEntry ? new GuiConfigEntries.CategoryEntry(this, entryList, entry.getConfigElement()) {
            @Nonnull
            @Override
            protected GuiScreen buildChildScreen() {
                return new GuiComponentConfig(owningScreen, configElement.getChildElements(), owningScreen.modID,
                        owningScreen.allRequireWorldRestart || configElement.requiresWorldRestart(), owningScreen.allRequireMcRestart || configElement.requiresMcRestart(),
                        owningScreen.title, ((owningScreen.titleLine2 == null ? "" : owningScreen.titleLine2) + " > " + name));
            }

            @Override
            public boolean enabled() { return !configElement.getChildElements().isEmpty(); }
        } : new ComponentConfigEntry(entry));
    }
}

/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.client.config.gui;

import git.jbredwards.fluidlogged_api.mod.client.config.element.ComponentConfigEntry;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.ScaledResolution;
import net.minecraft.client.renderer.Tessellator;
import net.minecraftforge.fml.client.config.*;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import org.lwjgl.opengl.GL11;

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
    @Nonnull
    protected ConfigScreenBuilder childScreenBuilder;
    public GuiComponentConfig(@Nonnull final GuiScreen parentScreen, @Nonnull final List<IConfigElement> configElements, @Nonnull final String modID,
                              final boolean allRequireWorldRestart, final boolean allRequireMcRestart, @Nonnull final String title, @Nullable final String titleLine2) {
        super(parentScreen, configElements, modID, allRequireWorldRestart, allRequireMcRestart, title, titleLine2);
        childScreenBuilder = GuiComponentConfig::new;
    }

    @Override
    public void initGui() {
        final boolean loadComponentEntries = entryList == null || needsRefresh;
        if(loadComponentEntries) {
            entryList = createEntryList();
            needsRefresh = false;
        }

        super.initGui();
        if(loadComponentEntries) entryList.listEntries.replaceAll(entry -> entry instanceof GuiConfigEntries.CategoryEntry
        ? new GuiConfigEntries.CategoryEntry(this, entryList, entry.getConfigElement()) {
            @Nonnull
            @Override
            protected GuiScreen buildChildScreen() {
                return childScreenBuilder.build(owningScreen, configElement.getChildElements(), owningScreen.modID,
                        owningScreen.allRequireWorldRestart || configElement.requiresWorldRestart(), owningScreen.allRequireMcRestart || configElement.requiresMcRestart(),
                        owningScreen.title, ((owningScreen.titleLine2 == null ? "" : owningScreen.titleLine2) + " > " + name));
            }

            @Override
            public boolean enabled() { return !configElement.getChildElements().isEmpty(); }
        } : createConfigEntry(entry));
    }

    @Nullable
    protected GuiConfigEntries createEntryList() {
        return new GuiConfigEntries(this, mc) {
            @Override
            protected void overlayBackground(final int startY, final int endY, final int startAlpha, final int endAlpha) {
                if(mc.world != null) drawGradientRect(left, endY, left + width, startY, -1072689136, -804253680);
                else super.overlayBackground(startY, endY, startAlpha, endAlpha);
            }

            @Override
            protected void drawContainerBackground(@Nonnull final Tessellator tessellator) {
                if(mc.world != null) drawGradientRect(right, bottom, left, top, -1072689136, -804253680);
                else super.drawContainerBackground(tessellator);
            }

            @Override
            protected void drawSelectionBox(final int insideLeft, final int insideTop, final int mouseXIn, final int mouseYIn, final float partialTicks) {
                final double scaleH = mc.displayHeight / new ScaledResolution(mc).getScaledHeight_double();
                GL11.glEnable(GL11.GL_SCISSOR_TEST);
                GL11.glScissor(0, (int)(mc.displayHeight - (bottom * scaleH)), mc.displayWidth, (int)((bottom - top) * scaleH));
                super.drawSelectionBox(insideLeft, insideTop, mouseXIn, mouseYIn, partialTicks);
                GL11.glDisable(GL11.GL_SCISSOR_TEST);
            }
        };
    }

    @Nonnull
    protected GuiConfigEntries.IConfigEntry createConfigEntry(@Nonnull final GuiConfigEntries.IConfigEntry original) {
        return new ComponentConfigEntry(original);
    }

    @FunctionalInterface
    public interface ConfigScreenBuilder {
        @Nonnull GuiScreen build(@Nonnull final GuiScreen parentScreen, @Nonnull final List<IConfigElement> configElements, @Nonnull final String modID,
                                 final boolean allRequireWorldRestart, final boolean allRequireMcRestart, @Nonnull final String title, @Nullable final String titleLine2);
    }
}

/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.client.gui;

import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.client.config.ConfigGuiType;
import net.minecraftforge.fml.client.config.GuiConfigEntries;
import net.minecraftforge.fml.client.config.IConfigElement;
import net.minecraftforge.fml.relauncher.ReflectionHelper;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;

/**
 *
 * @author jbred
 *
 */
public class ComponentConfigEntry implements GuiConfigEntries.IConfigEntry
{
    @Nonnull protected static final Field NAME = ReflectionHelper.findField(GuiConfigEntries.ListEntryBase.class, "name");
    @Nonnull protected static final Field TOOLTIP = ReflectionHelper.findField(GuiConfigEntries.ListEntryBase.class, "toolTip");

    @Nonnull protected final GuiConfigEntries.IConfigEntry parent;
    @Nonnull protected final ITextComponent component;

    public ComponentConfigEntry(@Nonnull final GuiConfigEntries.IConfigEntry parentIn) {
        if(parentIn instanceof GuiConfigEntries.ListEntryBase) {
            @Nonnull final String oldName = (String)get(NAME, parentIn);
            @Nonnull final String newName = (component = ForgeHooks.newChatWithLinks(oldName)).getFormattedText();

            set(NAME, parentIn, newName);
            @Nonnull final List<String> tooltips = (List<String>)get(TOOLTIP, parentIn);
            for(@Nonnull final ListIterator<String> it = tooltips.listIterator(); it.hasNext();) {
                @Nonnull final String tooltip = it.next();
                @Nonnull final ITextComponent tooltipComponent = ForgeHooks.newChatWithLinks(tooltip);

                component.appendSibling(tooltipComponent);
                tooltipComponent.getSiblings().forEach(component::appendSibling);
                it.set(tooltipComponent.getFormattedText().replace(oldName, newName));
            }

            if(parentIn.getConfigElement().getType() != ConfigGuiType.CONFIG_CATEGORY) {
                tooltips.remove(tooltips.size() - 1);
                tooltips.add(TextFormatting.AQUA + I18n.format("fml.configgui.tooltip.default", I18n.format("configgui.fluidloggedAPI.default." + parentIn.getConfigElement().getDefault().toString())));
            }
        }

        else component = ForgeHooks.newChatWithLinks(parentIn.getName()).appendSibling(ForgeHooks.newChatWithLinks(parentIn.getConfigElement().getComment()));
        parent = parentIn;
    }

    @Override
    public boolean mousePressed(final int slotIndex, final int mouseX, final int mouseY, final int mouseEvent, final int relativeX, final int relativeY) {
        if(parent.mousePressed(slotIndex, mouseX, mouseY, mouseEvent, relativeX, relativeY)) return true;
        @Nonnull final GuiScreen openGui = Objects.requireNonNull(FMLClientHandler.instance().getClient().currentScreen);

        for(@Nonnull final ITextComponent sibling : component.getSiblings()) if(openGui.handleComponentClick(sibling)) return true;
        return openGui.handleComponentClick(component);
    }

    // =====
    // MIMIC
    // =====

    @Nonnull
    @Override
    public IConfigElement getConfigElement() { return parent.getConfigElement(); }

    @Nonnull
    @Override
    public String getName() { return parent.getName(); }

    @Nonnull
    @Override
    public Object getCurrentValue() { return parent.getCurrentValue(); }

    @Nonnull
    @Override
    public Object[] getCurrentValues() { return parent.getCurrentValues(); }

    @Override
    public boolean enabled() { return parent.enabled(); }

    @Override
    public void keyTyped(final char eventChar, final int eventKey) { parent.keyTyped(eventChar, eventKey); }

    @Override
    public void updateCursorCounter() { parent.updateCursorCounter(); }

    @Override
    public void mouseClicked(final int x, final int y, final int mouseEvent) { parent.mouseClicked(x, y, mouseEvent); }

    @Override
    public boolean isDefault() { return parent.isDefault(); }

    @Override
    public void setToDefault() { parent.setToDefault(); }

    @Override
    public void undoChanges() { parent.undoChanges(); }

    @Override
    public boolean isChanged() { return parent.isChanged(); }

    @Override
    public boolean saveConfigElement() { return parent.saveConfigElement(); }

    @Override
    public void drawToolTip(final int mouseX, final int mouseY) { parent.drawToolTip(mouseX, mouseY); }

    @Override
    public int getLabelWidth() { return parent.getLabelWidth(); }

    @Override
    public int getEntryRightBound() { return parent.getEntryRightBound(); }

    @Override
    public void onGuiClosed() { parent.onGuiClosed(); }

    @Override
    public void updatePosition(final int slotIndex, final int x, final int y, final float partialTicks) {
        parent.updatePosition(slotIndex, x, y, partialTicks);
    }

    @Override
    public void drawEntry(final int slotIndex, final int x, final int y, final int listWidth, final int slotHeight, final int mouseX, final int mouseY, final boolean isSelected, final float partialTicks) {
        parent.drawEntry(slotIndex, x, y, listWidth, slotHeight, mouseX, mouseY, isSelected, partialTicks);
    }

    @Override
    public void mouseReleased(final int slotIndex, final int x, final int y, final int mouseEvent, final int relativeX, final int relativeY) {
        parent.mouseReleased(slotIndex, x, y, mouseEvent, relativeX, relativeY);
    }

    // =============
    // FIELD HELPERS
    // =============

    @Nonnull
    protected static Object get(@Nonnull final Field field, @Nonnull final Object obj) {
        try { return field.get(obj); }
        catch(@Nonnull final IllegalAccessException e) { throw new RuntimeException(e); }
    }

    protected static void set(@Nonnull final Field field, @Nonnull final Object obj, @Nonnull final Object value) {
        try { field.set(obj, value); }
        catch(@Nonnull final IllegalAccessException e) { throw new RuntimeException(e); }
    }
}

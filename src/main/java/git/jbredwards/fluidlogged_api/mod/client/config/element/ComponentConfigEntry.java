/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.client.config.element;

import net.minecraft.client.Minecraft;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.fml.client.config.ConfigGuiType;
import net.minecraftforge.fml.client.config.GuiConfigEntries;
import net.minecraftforge.fml.client.config.IConfigElement;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;

/**
 *
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class ComponentConfigEntry implements GuiConfigEntries.IConfigEntry
{
    @Nonnull protected static final Field NAME = ReflectionHelper.findField(GuiConfigEntries.ListEntryBase.class, "name");
    @Nonnull protected static final Field TOOLTIP = ReflectionHelper.findField(GuiConfigEntries.ListEntryBase.class, "toolTip");

    @Nonnull protected final GuiConfigEntries.IConfigEntry parent;
    @Nonnull protected final ITextComponent component;

    public ComponentConfigEntry(@Nonnull final GuiConfigEntries.IConfigEntry parentIn) {
        if(parentIn instanceof GuiConfigEntries.ListEntryBase) {
            @Nonnull final String oldName = get(NAME, parentIn).toString();
            @Nonnull final String newName = (component = ForgeHooks.newChatWithLinks(oldName)).getFormattedText();

            set(NAME, parentIn, newName);
            @Nonnull final List<String> tooltips = (List<String>)get(TOOLTIP, parentIn);
            for(@Nonnull final ListIterator<String> it = tooltips.listIterator(); it.hasNext();) {
                @Nonnull final ITextComponent tooltipComponent = ForgeHooks.newChatWithLinks(it.next());

                component.appendSibling(tooltipComponent);
                tooltipComponent.getSiblings().forEach(component::appendSibling);
                it.set(tooltipComponent.getFormattedText().replace(oldName, newName));
            }

            @Nonnull final IConfigElement element = parentIn.getConfigElement();
            if(element.getType() != ConfigGuiType.CONFIG_CATEGORY && !element.isList()) {
                tooltips.remove(tooltips.size() - 1);
                tooltips.add(TextFormatting.AQUA + I18n.format("fml.configgui.tooltip.default", Optional.ofNullable(element.getDefault()).map(o -> {
                    if(element.getType() == ConfigGuiType.STRING) return I18n.format(element.getLanguageKey() + '.' + o);
                    else return element.getType() == ConfigGuiType.BOOLEAN ? I18n.format(o.toString()) : o.toString();
                }).orElse("[]")));
            }
        }

        else component = ForgeHooks.newChatWithLinks(parentIn.getName()).appendSibling(ForgeHooks.newChatWithLinks(parentIn.getConfigElement().getComment()));
        parent = parentIn;
    }

    @Override
    public void drawEntry(final int slotIndex, final int x, final int y, final int listWidth, final int slotHeight, final int mouseX, final int mouseY, final boolean isSelected, final float partialTicks) {
        @Nonnull final ToggleableConfigElement element = (ToggleableConfigElement)getConfigElement();
        element.enabled = enabled();
        parent.drawEntry(slotIndex, x, y, listWidth, slotHeight, mouseX, mouseY, isSelected, partialTicks);
        element.enabled = null;
    }

    @Override
    public boolean mousePressed(final int slotIndex, final int mouseX, final int mouseY, final int mouseEvent, final int relativeX, final int relativeY) {
        return parent.mousePressed(slotIndex, mouseX, mouseY, mouseEvent, relativeX, relativeY) || Optional.ofNullable(Minecraft.getMinecraft().currentScreen).map(openGui -> {
            for(@Nonnull final ITextComponent sibling : component.getSiblings()) if(openGui.handleComponentClick(sibling)) return Boolean.TRUE;
            return openGui.handleComponentClick(component);
        }).orElse(Boolean.FALSE);
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

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

import net.minecraftforge.fml.client.config.ConfigGuiType;
import net.minecraftforge.fml.client.config.GuiConfigEntries;
import net.minecraftforge.fml.client.config.GuiEditArrayEntries;
import net.minecraftforge.fml.client.config.IConfigElement;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 *
 * @author jbred
 *
 */
public class ToggleableConfigElement implements IConfigElement
{
    @Nonnull protected final IConfigElement parent;
    @Nonnull protected final ConfigElementWrapper wrapper;
    @Nullable public Boolean display, enabled;

    public ToggleableConfigElement(@Nonnull final IConfigElement parentIn, @Nonnull final ConfigElementWrapper wrapperIn) {
        parent = parentIn;
        wrapper = wrapperIn;
    }

    @Override
    public boolean requiresWorldRestart() {
        return enabled != null ? enabled.equals(Boolean.FALSE) : parent.requiresWorldRestart();
    }

    @Override
    public boolean showInGui() {
        return display != null ? display.equals(Boolean.TRUE) : parent.showInGui();
    }

    @Nonnull
    public IConfigElement getParent() {
        return parent instanceof ToggleableConfigElement ? ((ToggleableConfigElement)parent).getParent() : parent;
    }

    @Nonnull
    @Override
    public List<IConfigElement> getChildElements() {
        return parent.getChildElements().stream().map(element -> wrapper.wrap(element, wrapper)).collect(Collectors.toList());
    }

    @FunctionalInterface
    public interface ConfigElementWrapper {
        @Nonnull IConfigElement wrap(@Nonnull final IConfigElement element, @Nonnull final ConfigElementWrapper wrapper);
    }

    // =====
    // MIMIC
    // =====

    @Override
    public boolean isProperty() { return parent.isProperty(); }

    @Nonnull
    @Override
    public Class<? extends GuiConfigEntries.IConfigEntry> getConfigEntryClass() { return parent.getConfigEntryClass(); }

    @Nonnull
    @Override
    public Class<? extends GuiEditArrayEntries.IArrayEntry> getArrayEntryClass() { return parent.getArrayEntryClass(); }

    @Nonnull
    @Override
    public String getName() { return parent.getName(); }

    @Nonnull
    @Override
    public String getQualifiedName() { return parent.getQualifiedName(); }

    @Nonnull
    @Override
    public String getLanguageKey() { return parent.getLanguageKey(); }

    @Nullable
    @Override
    public String getComment() { return parent.getComment(); }

    @Nonnull
    @Override
    public ConfigGuiType getType() { return parent.getType(); }

    @Override
    public boolean isList() { return parent.isList(); }

    @Override
    public boolean isListLengthFixed() { return parent.isListLengthFixed(); }

    @Override
    public int getMaxListLength() { return parent.getMaxListLength(); }

    @Override
    public boolean isDefault() { return parent.isDefault(); }

    @Nullable
    @Override
    public Object getDefault() { return parent.getDefault(); }

    @Nonnull
    @Override
    public Object[] getDefaults() { return parent.getDefaults(); }

    @Override
    public void setToDefault() { parent.setToDefault(); }

    @Override
    public boolean requiresMcRestart() { return parent.requiresMcRestart(); }

    @Override
    public boolean hasSlidingControl() { return parent.hasSlidingControl(); }

    @Nonnull
    @Override
    public Object get() { return parent.get(); }

    @Nonnull
    @Override
    public Object[] getList() { return parent.getList(); }

    @Override
    public void set(@Nonnull final Object value) { parent.set(value); }

    @Override
    public void set(@Nonnull final Object[] aVal) { parent.set(aVal); }

    @Nonnull
    @Override
    public String[] getValidValues() { return parent.getValidValues(); }

    @Nullable
    @Override
    public String[] getValidValuesDisplay() { return parent.getValidValuesDisplay(); }

    @Nonnull
    @Override
    public Object getMinValue() { return parent.getMinValue(); }

    @Nonnull
    @Override
    public Object getMaxValue() { return parent.getMaxValue(); }

    @Nullable
    @Override
    public Pattern getValidationPattern() { return parent.getValidationPattern(); }
}

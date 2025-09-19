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

package git.jbredwards.fluidlogged_api.mod.common.command;

import net.minecraft.command.CommandBase;
import net.minecraft.command.ICommand;
import net.minecraft.command.ICommandSender;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 *
 * @author jbred
 *
 */
public abstract class CommandChildBase extends CommandBase
{
    @Nonnull
    protected final String name, usage;
    protected CommandChildBase(@Nullable final ICommand parentIn, @Nonnull final String nameIn) {
        name = nameIn;
        usage = "commands.fluidlogged_api." + (parentIn == null ? nameIn : (parentIn.getName() + "." + nameIn)) + ".usage";
    }

    @Nonnull
    @Override
    public String getName() { return name; }

    @Nonnull
    @Override
    public String getUsage(@Nonnull final ICommandSender sender) { return usage; }
}

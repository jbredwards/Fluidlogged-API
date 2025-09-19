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

import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import net.minecraft.command.*;
import net.minecraft.server.MinecraftServer;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;

/**
 *
 * @author jbred
 *
 */
public class CommandReloadConfig extends CommandChildBase
{
    public CommandReloadConfig(@Nullable final ICommand parentIn, @Nonnull final String nameIn) {
        super(parentIn, nameIn);
    }

    @Override
    public void execute(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args) throws CommandException {
        if(args.length != 0) throw new WrongUsageException(getUsage(sender));
        else try {
            notifyCommandListener(sender, this, "commands.fluidlogged_api.reloadFluidloggedAPI.start");
            FluidloggedAPIConfigs.initConfigs(server, true);
            notifyCommandListener(sender, this, "commands.fluidlogged_api.generic.finished");
        }

        // oops
        catch(@Nonnull final IOException e) {
            e.printStackTrace();
            throw new CommandException(e.getMessage());
        }
    }

    @Nonnull
    @Override
    public String getName() { return name; }
}

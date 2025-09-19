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

import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.server.command.CommandTreeBase;
import net.minecraftforge.server.command.CommandTreeHelp;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
public final class CommandFluidloggedAPI extends CommandTreeBase
{
    @Nonnull
    private final String name;
    public CommandFluidloggedAPI(@Nonnull final String nameIn) {
        name = nameIn;
        addSubcommand(new CommandPrint(this));
        addSubcommand(new CommandReloadConfig(this, "reload"));
        addSubcommand(new CommandSetFluidState(this));
        addSubcommand(new CommandTest(this));
        addSubcommand(new CommandTreeHelp(this));
    }

    @Override
    public int getRequiredPermissionLevel() { return 0; }

    @Nonnull
    @Override
    public String getName() { return name; }

    @Nonnull
    @Override
    public String getUsage(@Nonnull final ICommandSender sender) { return "commands.fluidlogged_api." + getName() + ".usage"; }

    @Override
    public boolean checkPermission(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender) { return true; }
}

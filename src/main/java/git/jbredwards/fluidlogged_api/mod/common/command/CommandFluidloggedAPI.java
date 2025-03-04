/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
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

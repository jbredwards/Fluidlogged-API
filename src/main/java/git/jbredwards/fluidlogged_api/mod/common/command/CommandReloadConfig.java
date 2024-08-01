/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.command;

import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author jbred
 *
 */
public class CommandReloadConfig extends CommandBase
{
    @Nonnull
    @Override
    public String getName() { return "reloadFluidloggedAPI"; }

    @Nonnull
    @Override
    public String getUsage(@Nonnull final ICommandSender sender) { return "commands.reloadFluidloggedAPI.usage"; }

    @Override
    public void execute(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args) throws CommandException {
        if(args.length != argSize() || (args.length == 1 && !args[0].equals("reload"))) throw new WrongUsageException(getUsage(sender));
        else try {
            // don't allow the server to reload a common-side config while more than
            // just the host is online, doing this would result all changes causing a desync!
            if(server.getPlayerList().getCurrentPlayerCount() > 1) throw new WrongUsageException("commands.reloadFluidloggedAPI.desyncWarning");
            synchronized(this) { FluidloggedAPIConfigs.initConfigs(true); }
            notifyCommandListener(sender, this, "commands.reloadFluidloggedAPI.success");
        }

        // oops
        catch(@Nonnull final IOException e) {
            e.printStackTrace();
            throw new CommandException(e.getMessage());
        }
    }

    @Nonnull
    @Override
    public List<String> getTabCompletions(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args, @Nullable final BlockPos targetPos) {
        return args.length <= argSize() ? getListOfStringsMatchingLastWord(args, args.length == 0 ? getName() : "reload") : Collections.emptyList();
    }

    // an alternate command that performs the same function
    public static class Trimmed extends CommandReloadConfig
    {
        @Override
        protected int argSize() { return 1; }

        @Nonnull
        @Override
        public String getName() { return "fluidlogged"; }

        @Nonnull
        @Override
        public String getUsage(@Nonnull final ICommandSender sender) { return "commands.fluidlogged.usage"; }
    }

    // an alternate command that performs the same function
    public static class TrimmedAPI extends CommandReloadConfig
    {
        @Override
        protected int argSize() { return 1; }

        @Nonnull
        @Override
        public String getName() { return "fluidloggedAPI"; }

        @Nonnull
        @Override
        public String getUsage(@Nonnull final ICommandSender sender) { return "commands.fluidloggedAPI.usage"; }
    }

    // allows for alternate commands to exist
    protected int argSize() { return 0; }
}

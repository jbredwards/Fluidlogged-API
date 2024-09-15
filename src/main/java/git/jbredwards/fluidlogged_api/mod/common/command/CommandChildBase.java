/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
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

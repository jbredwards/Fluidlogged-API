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

import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.command.*;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;

/**
 *
 * @author jbred
 *
 */
public class CommandTest extends CommandChildBase
{
    public CommandTest(@Nullable final ICommand parentIn) { super(parentIn, "test"); }

    @Override
    public int getRequiredPermissionLevel() { return 1; }

    @Override
    public void execute(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args) throws CommandException {
        if(args.length != 4 && args.length != 5) throw new WrongUsageException(getUsage(sender));

        @Nonnull final BlockPos pos = parseBlockPos(sender, args, 0, false);
        @Nonnull final World world = sender.getEntityWorld();
        if(!world.isBlockLoaded(pos)) throw new CommandException("commands.fluidlogged_api.test.outOfWorld");

        @Nonnull final Block block = getBlockByText(sender, args[3]);
        @Nonnull final FluidState fluidState = args.length == 5 ? FluidState.of(convertArgToBlockState(block, args[4])) : FluidState.of(block);

        if(!fluidState.isValid()) throw new WrongUsageException(getUsage(sender));
        else if(!FluidloggedUtils.isStateFluidloggable(world.getBlockState(pos), world, pos, fluidState)) throw new CommandException("commands.fluidlogged_api.test.fail");
        else notifyCommandListener(sender, this, "commands.fluidlogged_api.test.success");
    }

    @Nonnull
    @Override
    public List<String> getTabCompletions(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args, @Nullable final BlockPos targetPos) {
        if(args.length < 4) return args.length == 0 ? getListOfStringsMatchingLastWord(new String[0], getName()) : getTabCompletionCoordinate(args, 0, targetPos);
        @Nonnull final List<String> matching = new CommandSetFluidState(null).getTabCompletions(server, sender, args, targetPos);

        matching.remove("minecraft:air");
        return matching;
    }
}

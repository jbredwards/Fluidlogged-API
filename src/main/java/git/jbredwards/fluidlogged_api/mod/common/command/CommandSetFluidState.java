/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.command;

import git.jbredwards.fluidlogged_api.api.fluid.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.command.*;
import net.minecraft.init.Blocks;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 *
 * @author jbred
 *
 */
public class CommandSetFluidState extends CommandChildBase
{
    @Nullable
    protected static ResourceLocation[] TAB_COMPLETIONS = null;
    public CommandSetFluidState(@Nullable final ICommand parentIn) { super(parentIn, "setfluid"); }

    @Override
    public int getRequiredPermissionLevel() { return 2; }

    @Override
    public void execute(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args) throws CommandException {
        if(args.length != 4 && args.length != 5) throw new WrongUsageException(getUsage(sender));
        sender.setCommandStat(CommandResultStats.Type.AFFECTED_BLOCKS, 0);

        @Nonnull final BlockPos pos = parseBlockPos(sender, args, 0, false);
        @Nonnull final World world = sender.getEntityWorld();
        if(!world.isBlockLoaded(pos)) throw new CommandException("commands.fluidlogged_api.setfluid.outOfWorld");

        @Nonnull final Block block = getBlockByText(sender, args[3]);
        @Nonnull final FluidState fluidState = args.length == 5 ? FluidState.of(convertArgToBlockState(block, args[4])) : FluidState.of(block);
        if(block != Blocks.AIR && !fluidState.isValid()) throw new WrongUsageException(getUsage(sender));

        @Nonnull final IBlockState here = world.getBlockState(pos);
        if(fluidState.isEmpty() || FluidloggedUtils.isStateFluidloggable(here, world, pos, fluidState)) {
            if(!FluidloggedUtils.setFluidState(world, pos, here, fluidState, false, Constants.BlockFlags.DEFAULT))
                throw new CommandException("commands.fluidlogged_api.setfluid.noChange");
        }

        else if(fluidState.isEmpty() && !FluidloggedUtils.isFluid(here) || !world.setBlockState(pos, fluidState.getState())) {
            throw new CommandException("commands.fluidlogged_api.setfluid.noChange");
        }

        notifyCommandListener(sender, this, fluidState.isEmpty() ? "commands.fluidlogged_api.setfluid.successAir" : "commands.fluidlogged_api.setfluid.successFluid");
        sender.setCommandStat(CommandResultStats.Type.AFFECTED_BLOCKS, 1);
    }

    @Nonnull
    @Override
    public List<String> getTabCompletions(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args, @Nullable final BlockPos targetPos) {
        if(args.length < 4) return args.length == 0 ? getListOfStringsMatchingLastWord(new String[0], getName()) : getTabCompletionCoordinate(args, 0, targetPos);
        else if(args.length > 4) return Collections.emptyList();
        else if(TAB_COMPLETIONS == null) {
            @Nonnull final ResourceLocation[] fluidBlocks = FluidRegistry.getRegisteredFluids().values().stream()
                    .filter(fluid -> fluid.getBlock() instanceof IFluidloggableFluid && ((IFluidloggableFluid)fluid.getBlock()).isFluidloggableFluid(FluidState.of(fluid)))
                    .map(fluid -> fluid.getBlock().getRegistryName())
                    .filter(Objects::nonNull)
                    .toArray(ResourceLocation[]::new);

            TAB_COMPLETIONS = new ResourceLocation[fluidBlocks.length + 3];
            TAB_COMPLETIONS[fluidBlocks.length + 2] = new ResourceLocation("flowing_water");
            TAB_COMPLETIONS[fluidBlocks.length + 1] = new ResourceLocation("flowing_lava");
            TAB_COMPLETIONS[fluidBlocks.length] = new ResourceLocation("air");

            System.arraycopy(fluidBlocks, 0, TAB_COMPLETIONS, 0, fluidBlocks.length);
            Arrays.sort(TAB_COMPLETIONS);
        }

        return getListOfStringsMatchingLastWord(args, Arrays.asList(TAB_COMPLETIONS));
    }
}

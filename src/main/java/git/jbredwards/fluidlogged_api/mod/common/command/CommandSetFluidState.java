package git.jbredwards.fluidlogged_api.mod.common.command;

import com.google.common.collect.ImmutableList;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.state.IBlockState;
import net.minecraft.command.*;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidRegistry;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 *
 * @author jbred
 *
 */
public class CommandSetFluidState extends CommandBase
{
    @Nonnull
    protected static final List<ResourceLocation> TAB_COMPLETIONS = ImmutableList.<ResourceLocation>builder()
            .add(FluidRegistry.getRegisteredFluids().values().stream()
                    .filter(fluid -> FluidloggedUtils.isFluidloggableFluid(fluid.getBlock()))
                    .map(fluid -> fluid.getBlock().getRegistryName())
                    .filter(Objects::nonNull)
                    .toArray(ResourceLocation[]::new))
            .add(new ResourceLocation("air"))
            .build();

    @Nonnull
    @Override
    public String getName() { return "setfluid"; }

    @Override
    public int getRequiredPermissionLevel() { return 2; }

    @Nonnull
    @Override
    public String getUsage(@Nonnull ICommandSender sender) { return "commands.setfluid.usage"; }

    @Override
    public void execute(@Nonnull MinecraftServer server, @Nonnull ICommandSender sender, @Nonnull String[] args) throws CommandException {
        if(args.length < 4) throw new WrongUsageException("commands.setfluid.usage");
        sender.setCommandStat(CommandResultStats.Type.AFFECTED_BLOCKS, 0);

        final BlockPos pos = parseBlockPos(sender, args, 0, false);

        final World world = sender.getEntityWorld();
        if(!world.isBlockLoaded(pos)) throw new CommandException("commands.setfluid.outOfWorld");

        final FluidState fluidState = FluidState.of(getBlockByText(sender, args[3]));
        final IBlockState here = world.getBlockState(pos);

        if(FluidloggedUtils.isStateFluidloggable(here, world, pos, fluidState.getFluid())) {
            if(!FluidloggedUtils.setFluidState(world, pos, here, fluidState, false, true, 2))
                throw new CommandException("commands.setfluid.noChange");
        }

        else if(!world.setBlockState(pos, fluidState.getState(), 2))
            throw new CommandException("commands.setfluid.noChange");

        notifyCommandListener(sender, this, "commands.setfluid.success");
    }

    @Nonnull
    @Override
    public List<String> getTabCompletions(@Nonnull MinecraftServer server, @Nonnull ICommandSender sender, @Nonnull String[] args, @Nullable BlockPos targetPos) {
        if(args.length > 0 && args.length <= 3) return getTabCompletionCoordinate(args, 0, targetPos);
        return args.length > 4 ? Collections.emptyList() : getListOfStringsMatchingLastWord(args, TAB_COMPLETIONS);
    }
}

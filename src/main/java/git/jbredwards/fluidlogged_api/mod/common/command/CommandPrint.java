/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.command;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import git.jbredwards.fluidlogged_api.api.block.IFluidloggable;
import git.jbredwards.fluidlogged_api.api.event.FluidloggableEvent;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigs;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageCommandPrint;
import git.jbredwards.fluidlogged_api.mod.common.message.SMessageSyncRuntimeConfigs;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;
import joptsimple.util.EnumConverter;
import joptsimple.util.PathConverter;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommand;
import net.minecraft.command.ICommandSender;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.PacketBuffer;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.JsonUtils;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.world.chunk.BlockStateContainer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.eventhandler.EventBus;
import net.minecraftforge.fml.common.registry.ForgeRegistries;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import net.minecraftforge.fml.relauncher.Side;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.*;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

/**
 *
 * @author jbred
 *
 */
public class CommandPrint extends CommandChildBase
{
    @Nonnull protected final OptionParser parser = new OptionParser();
    @Nonnull protected final OptionSpec<Side> sideSpec = parser.accepts("side", "Side (client, server) to execute this command.")
            .withRequiredArg().withValuesConvertedBy(new EnumConverter<Side>(Side.class) {});
    @Nonnull protected final OptionSpec<ConfigType> configToSaveSpec = parser.accepts("configToSave", "Config type (all, blacklist, fluidTags, whitelist) to output.")
            .withRequiredArg().withValuesConvertedBy(ConfigType.DESERIALIZER);
    @Nonnull protected final OptionSpec<FluidloggableType> fluidloggableTypeSpec = parser.accepts("handlerType", "Fluidloggable handler type (all, builtin, config, listener) to output.")
            .withRequiredArg().withValuesConvertedBy(FluidloggableType.DESERIALIZER);
    @Nonnull protected final OptionSpec<Path> pathSpec = parser.accepts("path", "File path.")
            .withRequiredArg().withValuesConvertedBy(new PathConverter());

    protected static final int eventID = ReflectionHelper.getPrivateValue(EventBus.class, MinecraftForge.EVENT_BUS, "busID");
    public CommandPrint(@Nullable final ICommand parentIn) { super(parentIn, "print"); }

    @Override
    public void execute(@Nonnull final MinecraftServer server, @Nonnull final ICommandSender sender, @Nonnull final String[] args) throws CommandException {
        // optional command arguments
        parser.allowsUnrecognizedOptions();
        @Nonnull final OptionSet commandArgs = parser.parse(args);
        final boolean isPlayer = sender instanceof EntityPlayerMP;

        // require a different permission level to save file on server system
        @Nonnull final Side side = Optional.ofNullable(commandArgs.valueOf(sideSpec)).orElse(isPlayer ? Side.CLIENT : Side.SERVER);
        if(side.isServer() && !sender.canUseCommand(getServerRequiredPermissionLevel(), getName())) throw new CommandException("commands.generic.permission");

        // collect rest of args
        @Nonnull final ConfigType configToSave = Optional.ofNullable(commandArgs.valueOf(configToSaveSpec)).orElse(ConfigType.ALL);
        @Nonnull final FluidloggableType fluidloggableType = Optional.ofNullable(commandArgs.valueOf(fluidloggableTypeSpec)).orElse(FluidloggableType.ALL);
        @Nonnull final Path path = Optional.ofNullable(commandArgs.valueOf(pathSpec)).orElseGet(() -> Paths.get("fluidlogged_api/print_command_out"));

        // run command on server
        if(side.isServer() || isPlayer && !server.isDedicatedServer() && ((EntityPlayerMP)sender).connection.getNetworkManager().isLocalChannel()) {
            try {
                sender.sendMessage(new TextComponentTranslation("commands.fluidlogged_api.print.start"));
                fluidloggableType.save(path, fluidloggableType.getCommandArgs(configToSave, server));
                sender.sendMessage(new TextComponentTranslation("commands.fluidlogged_api.generic.finished"));
            }
            catch(@Nonnull final Exception e) {
                e.printStackTrace();
                throw new CommandException(e.getMessage());
            }
        }

        // run command on client
        else if(isPlayer) {
            sender.sendMessage(new TextComponentTranslation("commands.fluidlogged_api.print.start"));
            FluidloggedAPI.WRAPPER.sendTo(new SMessageCommandPrint(path, fluidloggableType, fluidloggableType.getCommandArgs(configToSave, server)), (EntityPlayerMP)sender);
        }

        // don't run command
        else throw new CommandException("commands.fluidlogged_api.print.badSender");
    }

    @Override
    public int getRequiredPermissionLevel() { return 2; }
    public int getServerRequiredPermissionLevel() { return super.getRequiredPermissionLevel(); }

    public enum ConfigType
    {
        ALL("configs") {
            @Nonnull
            @Override
            public JsonObject get(@Nonnull final JsonObject json) { return json; }
        },
        BLACKLIST("blacklist.json") {
            @Nonnull
            @Override
            public JsonObject get(@Nonnull final JsonObject json) { return JsonUtils.getJsonObject(json, "BLACKLIST"); }
        },
        FLUIDTAGS("fluidTags.json") {
            @Nonnull
            @Override
            public JsonObject get(@Nonnull final JsonObject json) { return JsonUtils.getJsonObject(json, "FLUID_TAGS"); }
        },
        WHITELIST("whitelist.json") {
            @Nonnull
            @Override
            public JsonObject get(@Nonnull final JsonObject json) { return JsonUtils.getJsonObject(json, "WHITELIST"); }
        };

        @Nonnull
        public static final EnumConverter<ConfigType> DESERIALIZER = new EnumConverter<ConfigType>(ConfigType.class) {
            @Nonnull
            @Override
            public ConfigType convert(@Nonnull final String value) {
                switch(value.toLowerCase()) {
                    case "b": return BLACKLIST;
                    case "f": return FLUIDTAGS;
                    case "w": return WHITELIST;
                }

                return super.convert(value);
            }
        };

        @Nonnull
        public final String fileName;
        ConfigType(@Nonnull final String fileNameIn) { fileName = fileNameIn; }

        @Nonnull
        public abstract JsonObject get(@Nonnull final JsonObject json);
    }

    public enum FluidloggableType
    {
        ALL {
            @Override
            public void save(@Nonnull final Path path, @Nonnull final Object[] args) throws Exception {
                for(int i = 1; i < values().length; i++) values()[i].save(path, (Object[])args[i - 1]);
            }

            @Nonnull
            @Override
            public Object[] getCommandArgs(@Nonnull final ConfigType configType, @Nonnull final MinecraftServer server) {
                @Nonnull final Object[][] args = new Object[values().length - 1][];
                for(int i = 1; i < values().length; i++) args[i - 1] = values()[i].getCommandArgs(configType, server);
                return args;
            }

            @Nonnull
            @Override
            public Object[] packetRead(@Nonnull final PacketBuffer buf) {
                @Nonnull final Object[][] args = new Object[values().length - 1][];
                for(int i = 1; i < values().length; i++) args[i - 1] = values()[i].packetRead(buf);
                return args;
            }

            @Override
            public void packetWrite(@Nonnull final PacketBuffer buf, @Nonnull final Object[] args) {
                for(int i = 1; i < values().length; i++) values()[i].packetWrite(buf, (Object[])args[i - 1]);
            }
        },
        EVENT {
            @Override
            public void save(@Nonnull final Path path, @Nonnull final Object[] args) throws Exception {
                Files.createDirectories(path);
                @Nonnull final FluidloggableEvent dummyEvent = new FluidloggableEvent(BlockStateContainer.AIR_BLOCK_STATE, null, BlockPos.ORIGIN, FluidState.EMPTY);
                @Nonnull final String[] lines = Arrays.stream(dummyEvent.getListenerList().getListeners(eventID)).flatMap(e -> Stream.of(e.toString(), "")).toArray(String[]::new);
                Files.write(path.resolve("listeners.txt"), Arrays.asList(lines), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            }
        },
        INTERFACE {
            @Override
            public void save(@Nonnull final Path path, @Nonnull final Object[] args) throws Exception {
                Files.createDirectories(path);
                @Nonnull final String[] collectedLines = ForgeRegistries.BLOCKS.getEntries().stream()
                        .filter(e -> e.getValue() instanceof IFluidloggable)
                        .map(e -> e.getKey().toString() + ", " + e.getValue().getClass().getName() + ", " + ((IFluidloggable)e.getValue()).overrideApplyDefaultsSetting())
                        .toArray(String[]::new);

                @Nonnull final String[] lines = new String[collectedLines.length + 1];
                lines[0] = "Block ID, Block Class, Overrides \"applyDefaults\"";
                System.arraycopy(collectedLines, 0, lines, 1, collectedLines.length);
                Files.write(path.resolve("builtin.csv"), Arrays.asList(lines), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            }
        },
        PREDICATE {
            @Override
            public void save(@Nonnull final Path path, @Nonnull final Object[] args) throws Exception {
                Files.createDirectories(path);

                @Nonnull final ConfigType configType = (ConfigType)args[0];
                @Nonnull final Gson gson = new GsonBuilder().setPrettyPrinting().create();

                if(configType != ConfigType.ALL) write(path, configType, (JsonObject)args[1], gson);
                else { // write all configs
                    @Nonnull final Path all = path.resolve(configType.fileName);
                    if(!Files.exists(all)) Files.createDirectory(all);
                    for(int i = 1; i < ConfigType.values().length; i++) write(all, ConfigType.values()[i], (JsonObject)args[1], gson);
                }
            }

            void write(@Nonnull final Path path, @Nonnull final ConfigType configType, @Nonnull final JsonObject json, @Nonnull final Gson gson) throws IOException {
                @Nonnull final Writer writer = Files.newBufferedWriter(path.resolve(configType.fileName));
                gson.toJson(configType.get(json), writer);
                writer.close();
            }

            @Nonnull
            @Override
            public Object[] getCommandArgs(@Nonnull final ConfigType configType, @Nonnull final MinecraftServer server) {
                @Nonnull final Object[] args = new Object[2];
                args[0] = configType;
                args[1] = FluidloggedAPIConfigs.readConfigFiles(server);
                return args;
            }

            @Nonnull
            @Override
            public Object[] packetRead(@Nonnull final PacketBuffer buf) {
                @Nonnull final Object[] args = new Object[2];
                args[0] = buf.readEnumValue(ConfigType.class);

                @Nonnull final SMessageSyncRuntimeConfigs msg = new SMessageSyncRuntimeConfigs();
                msg.read(buf);
                args[1] = msg.configs;
                return args;
            }

            @Override
            public void packetWrite(@Nonnull final PacketBuffer buf, @Nonnull final Object[] args) {
                new SMessageSyncRuntimeConfigs((JsonObject)args[1]).write(buf.writeEnumValue((ConfigType)args[0]));
            }
        };

        @Nonnull
        public static final EnumConverter<FluidloggableType> DESERIALIZER = new EnumConverter<FluidloggableType>(FluidloggableType.class) {
            @Nonnull
            @Override
            public FluidloggableType convert(@Nonnull final String value) {
                switch(value.toLowerCase()) {
                    case "l":
                    case "listeners":
                    case "e":
                    case "fluidloggableevent":
                    case "event": return EVENT;
                    case "i":
                    case "b":
                    case "ifluidloggable":
                    case "builtin": return INTERFACE;
                    case "p":
                    case "c":
                    case "config": return PREDICATE;
                }

                return super.convert(value);
            }
        };

        @Nonnull
        public Object[] getCommandArgs(@Nonnull final ConfigType configType, @Nonnull final MinecraftServer server) { return new Object[0]; }
        public abstract void save(@Nonnull final Path path, @Nonnull final Object[] args) throws Exception;

        @Nonnull
        public Object[] packetRead(@Nonnull final PacketBuffer buf) { return new Object[0]; }
        public void packetWrite(@Nonnull final PacketBuffer buf, @Nonnull final Object[] args) {}
    }
}

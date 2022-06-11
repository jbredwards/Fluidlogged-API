package git.jbredwards.fluidlogged_api.mod.common.command;

import git.jbredwards.fluidlogged_api.mod.Main;
import git.jbredwards.fluidlogged_api.mod.common.config.ConfigHandler;
import git.jbredwards.fluidlogged_api.mod.common.message.ReloadCfgMessage;
import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.server.MinecraftServer;

import javax.annotation.Nonnull;
import java.io.IOException;

/**
 *
 * @author jbred
 *
 */
public class CommandReloadCfg extends CommandBase
{
    @Nonnull
    @Override
    public String getName() { return "reloadFluidloggedAPI"; }

    @Nonnull
    @Override
    public String getUsage(@Nonnull ICommandSender sender) { return "command.reloadFluidloggedAPI.usage"; }

    @Override
    public void execute(@Nonnull MinecraftServer server, @Nonnull ICommandSender sender, @Nonnull String[] args) throws CommandException {
        if(args.length > 0) throw new WrongUsageException(getUsage(sender));
        else try {
            ConfigHandler.init();
            ConfigHandler.complete();
            Main.wrapper.sendToServer(new ReloadCfgMessage());
            notifyCommandListener(sender, this, "commands.reloadFluidloggedAPI.success");
        }
        //oops
        catch(IOException e) { e.printStackTrace(); }
    }
}

package git.jbredwards.fluidlogged_api.mod.client.exception;

import git.jbredwards.fluidlogged_api.mod.FluidloggedAPI;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.GuiErrorScreen;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.text.TextFormatting;
import net.minecraftforge.fml.client.CustomModLoadingErrorDisplayException;
import net.minecraftforge.fml.client.FMLClientHandler;
import net.minecraftforge.fml.common.DummyModContainer;
import net.minecraftforge.fml.common.LoaderException;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;

/**
 *
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class UnsupportedOptifineException extends CustomModLoadingErrorDisplayException
{
    @Nonnull
    protected final String currentVersion;
    public UnsupportedOptifineException(@Nonnull String currentVersionIn) {
        currentVersion = currentVersionIn.replaceAll("1.12.2_", "").replaceAll("_MOD", "");
    }

    @Override
    public void initGui(@Nonnull GuiErrorScreen errorScreen, @Nonnull FontRenderer fontRenderer) {}

    @Override
    public void drawScreen(@Nonnull GuiErrorScreen errorScreen, @Nonnull FontRenderer fontRenderer, int mouseRelX, int mouseRelY, float tickTime) {
        final String modMissingDependenciesText = I18n.format("fml.messages.mod.missing.dependencies.compatibility", TextFormatting.BOLD + FluidloggedAPI.NAME + TextFormatting.RESET);
        errorScreen.drawCenteredString(fontRenderer, modMissingDependenciesText, errorScreen.width / 2, 75, 0xFFFFFF);

        final String missingReason = I18n.format("fml.messages.mod.missing.dependencies.you.have", currentVersion);
        final String versionInfoText = String.format(TextFormatting.BOLD + "%s " + TextFormatting.RESET + "(%s)", "OptiFine_HD_U_G5+", missingReason);
        final String message = I18n.format("fml.messages.mod.missing.dependencies.compatible.with", versionInfoText);
        errorScreen.drawCenteredString(fontRenderer, message, errorScreen.width / 2, 90, 0xEEEEEE);
    }

    public static void checkOptifineVersion() {
        if(FMLClientHandler.instance().hasOptifine()) {
            final DummyModContainer container = ObfuscationReflectionHelper.getPrivateValue(FMLClientHandler.class, FMLClientHandler.instance(), "optifineContainer");
            if(!container.getVersion().startsWith("OptiFine_1.12.2_HD_U_G")) throw new LoaderException(new UnsupportedOptifineException(container.getVersion()));
        }
    }
}

package git.jbredwards.fluidlogged_api.mod.client.exception;

import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.GuiErrorScreen;
import net.minecraft.client.resources.I18n;
import net.minecraftforge.fml.client.CustomModLoadingErrorDisplayException;
import net.minecraftforge.fml.common.Loader;
import net.minecraftforge.fml.common.LoaderException;
import net.minecraftforge.fml.common.LoaderState;
import net.minecraftforge.fml.common.ModContainer;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Use this to make the game crash when a specific mod is installed
 * @author jbred
 *
 */
@SideOnly(Side.CLIENT)
public class UnsupportedModException extends CustomModLoadingErrorDisplayException
{
    @Nonnull
    protected final ModContainer[] incompatibleMods;
    public UnsupportedModException(@Nonnull ModContainer[] incompatibleModsIn) { incompatibleMods = incompatibleModsIn; }

    @Override
    public void initGui(@Nonnull GuiErrorScreen errorScreen, @Nonnull FontRenderer fontRenderer) {}

    @Override
    public void drawScreen(@Nonnull GuiErrorScreen errorScreen, @Nonnull FontRenderer fontRenderer, int mouseRelX, int mouseRelY, float tickTime) {
        int offset = Math.max(100 - incompatibleMods.length * 10, 25);

        final String incompatibleModsText = I18n.format("error.fluidloggedAPI.incompatibleMods");
        errorScreen.drawCenteredString(fontRenderer, incompatibleModsText, errorScreen.width / 2, offset, 0xFFFFFF);
        errorScreen.drawCenteredString(fontRenderer, incompatibleModsText.replaceAll(".", "-"), errorScreen.width / 2, offset += 10, 0xFFFFFF);

        for(ModContainer mod : incompatibleMods) {
            final String message = String.format("- %s (\"%s\")", mod.getName(), mod.getSource().getName());
            errorScreen.drawCenteredString(fontRenderer, message, errorScreen.width / 2, offset += 10, 0xEEEEEE);
        }
    }

    public static void crashIfPresent(@Nonnull String... modIds) {
        final Map<String, ModContainer> indexedModList = Loader.instance().getIndexedModList();
        final List<ModContainer> incompatibleMods = new ArrayList<>();

        for(String modid : modIds) {
            final ModContainer container = indexedModList.get(modid);
            if(container != null && Loader.instance().getModState(container) != LoaderState.ModState.DISABLED)
                incompatibleMods.add(container);
        }

        if(!incompatibleMods.isEmpty())
            throw new LoaderException(new UnsupportedModException(incompatibleMods.toArray(new ModContainer[0])));
    }
}

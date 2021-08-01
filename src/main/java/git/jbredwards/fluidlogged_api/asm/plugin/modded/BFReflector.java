package git.jbredwards.fluidlogged_api.asm.plugin.modded;

import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.BlockRendererDispatcher;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import javax.annotation.Nullable;
import java.lang.reflect.Method;

/**
 * stores better foliage methods
 * @author jbred
 *
 */
public enum BFReflector
{
    ;

    @Nullable public static Class<?> Hooks = null;

    @Nullable public static Method renderWorldBlock = null;
    @Nullable public static Method canRenderBlockInLayer = null;

    //loads the class & the methods
    public static boolean loaded = false;
    public static void load() {
        if(!loaded) {
            //better foliage loaded
            try {
                //this tries to find the better foliage class, will catch if it isn't there
                Hooks = Class.forName("mods.betterfoliage.client.Hooks");

                //sets the values for the methods
                renderWorldBlock = ObfuscationReflectionHelper.findMethod(Hooks, "renderWorldBlock", boolean.class, BlockRendererDispatcher.class, IBlockState.class, BlockPos.class, IBlockAccess.class, BufferBuilder.class, BlockRenderLayer.class);
                canRenderBlockInLayer = ObfuscationReflectionHelper.findMethod(Hooks, "canRenderBlockInLayer", boolean.class, Block.class, IBlockState.class, BlockRenderLayer.class);
                //makes the methods accessible
                if(renderWorldBlock != null) renderWorldBlock.setAccessible(true);
                if(canRenderBlockInLayer != null) canRenderBlockInLayer.setAccessible(true);
            }
            //better foliage not loaded
            catch(ClassNotFoundException ignored) {}

            loaded = true;
        }
    }
}

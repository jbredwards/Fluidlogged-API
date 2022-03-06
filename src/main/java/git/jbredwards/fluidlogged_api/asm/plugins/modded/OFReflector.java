package git.jbredwards.fluidlogged_api.asm.plugins.modded;

import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.RegionRenderCacheBuilder;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import java.lang.reflect.Method;

/**
 * stores optifine methods (only gets called if optifine is installed)
 * @author jbred
 *
 */
public enum OFReflector
{
    ;

    public static Method setBlockLayer;
    public static Method getRenderEnv;
    public static Method setRegionRenderCacheBuilder;
    public static Method isOverlaysRendered;
    public static Method postRenderOverlays;
    public static Method setOverlaysRendered;


    static {
        try {
            final Class<?> RenderEnv = Class.forName("net.optifine.render.RenderEnv");

            //sets the values for the methods
            setBlockLayer = ObfuscationReflectionHelper.findMethod(BufferBuilder.class, "setBlockLayer", void.class, BlockRenderLayer.class);
            getRenderEnv = ObfuscationReflectionHelper.findMethod(BufferBuilder.class, "getRenderEnv", RenderEnv, IBlockState.class, BlockPos.class);
            setRegionRenderCacheBuilder = ObfuscationReflectionHelper.findMethod(RenderEnv, "setRegionRenderCacheBuilder", void.class, RegionRenderCacheBuilder.class);
            isOverlaysRendered = ObfuscationReflectionHelper.findMethod(RenderEnv, "isOverlaysRendered", boolean.class);
            postRenderOverlays = ObfuscationReflectionHelper.findMethod(RenderChunk.class, "postRenderOverlays", void.class, RegionRenderCacheBuilder.class, CompiledChunk.class, boolean[].class);
            setOverlaysRendered = ObfuscationReflectionHelper.findMethod(RenderEnv, "setOverlaysRendered", void.class, boolean.class);

            //makes the methods accessible
            setBlockLayer.setAccessible(true);
            getRenderEnv.setAccessible(true);
            setRegionRenderCacheBuilder.setAccessible(true);
            isOverlaysRendered.setAccessible(true);
            postRenderOverlays.setAccessible(true);
            setOverlaysRendered.setAccessible(true);

        }
        catch(ClassNotFoundException ignored) {}
    }
}

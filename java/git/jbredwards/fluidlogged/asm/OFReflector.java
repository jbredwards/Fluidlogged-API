package git.jbredwards.fluidlogged.asm;

import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.RegionRenderCacheBuilder;
import net.minecraft.client.renderer.chunk.CompiledChunk;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import javax.annotation.Nullable;
import java.lang.reflect.Method;

/**
 * holds some optifine methods used in the RenderChunkPlugin
 * @author jbred
 *
 */
public enum OFReflector
{
    ;

    @Nullable public static Class RenderEnv = null;

    @Nullable public static Method setBlockLayer = null;
    @Nullable public static Method getRenderEnv = null;
    @Nullable public static Method setRegionRenderCacheBuilder = null;
    @Nullable public static Method isOverlaysRendered = null;
    @Nullable public static Method postRenderOverlays = null;
    @Nullable public static Method setOverlaysRendered = null;

    //loads the methods & fields
    private static boolean loaded = false;
    public static void load() {
        if(!loaded) {
            //optifine loaded
            try {
                //this tries to find an optifine class, will catch if it isn't there
                RenderEnv = Class.forName("optifine.RenderEnv");

                //sets the values for the optifine-only methods
                setBlockLayer = ObfuscationReflectionHelper.findMethod(BufferBuilder.class, "setBlockLayer", void.class, BlockRenderLayer.class);
                getRenderEnv = ObfuscationReflectionHelper.findMethod(BufferBuilder.class, "getRenderEnv", RenderEnv, IBlockAccess.class, IBlockState.class, BlockPos.class);
                setRegionRenderCacheBuilder = ObfuscationReflectionHelper.findMethod(RenderEnv, "setRegionRenderCacheBuilder", void.class, RegionRenderCacheBuilder.class);
                isOverlaysRendered = ObfuscationReflectionHelper.findMethod(RenderEnv, "isOverlaysRendered", boolean.class);
                postRenderOverlays = ObfuscationReflectionHelper.findMethod(RenderChunk.class, "postRenderOverlays", void.class, RegionRenderCacheBuilder.class, CompiledChunk.class, boolean[].class);
                setOverlaysRendered = ObfuscationReflectionHelper.findMethod(RenderEnv, "setOverlaysRendered", void.class, boolean.class);

                //makes the methods accessible
                if(setBlockLayer != null) setBlockLayer.setAccessible(true);
                if(getRenderEnv != null) getRenderEnv.setAccessible(true);
                if(setRegionRenderCacheBuilder != null) setRegionRenderCacheBuilder.setAccessible(true);
                if(isOverlaysRendered != null) isOverlaysRendered.setAccessible(true);
                if(postRenderOverlays != null) postRenderOverlays.setAccessible(true);
                if(setOverlaysRendered != null) setOverlaysRendered.setAccessible(true);
            }
            //optifine not loaded
            catch(ClassNotFoundException ignored) {}

            loaded = true;
        }
    }
}

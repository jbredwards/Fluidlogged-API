package git.jbredwards.fluidlogged_api.client.util;

import git.jbredwards.fluidlogged_api.common.block.BlockFluidloggedTE;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BlockModelRenderer;
import net.minecraft.client.renderer.BlockModelShapes;
import net.minecraft.client.renderer.BlockRendererDispatcher;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.block.model.IBakedModel;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.IResourceManager;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.WorldType;

/**
 * changes some rendering stuff for fluidlogged te's
 * @author jbred
 *
 */
@SuppressWarnings("NullableProblems")
public class FluidloggedBlockRendererDispatcher extends BlockRendererDispatcher
{
    public BlockRendererDispatcher old;

    public FluidloggedBlockRendererDispatcher(BlockRendererDispatcher old) {
        super(old.getBlockModelShapes(), Minecraft.getMinecraft().getBlockColors());
        this.old = old;
    }

    //if the block is a fluidlogged te, render the fluid instead
    @Override
    public boolean renderBlock(IBlockState state, BlockPos pos, IBlockAccess blockAccess, BufferBuilder bufferBuilderIn) {
        //non-fluidlogged blocks
        if(!(state.getBlock() instanceof BlockFluidloggedTE)) return old.renderBlock(state, pos, blockAccess, bufferBuilderIn);
        //fluidlogged blocks
        else {
            if(blockAccess.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES) state = state.getActualState(blockAccess, pos);

            final IBakedModel model = getModelForState(((BlockFluidloggedTE)state.getBlock()).getFluid().getBlock().getDefaultState());
            state = state.getBlock().getExtendedState(state, blockAccess, pos);

            return getBlockModelRenderer().renderModel(blockAccess, model, state, pos, bufferBuilderIn, true);
        }
    }

    //if the block is a fluidlogged te, render the stored block's breaking animation instead
    @Override
    public void renderBlockDamage(IBlockState state, BlockPos pos, TextureAtlasSprite texture, IBlockAccess blockAccess) {
        if(!(state.getBlock() instanceof BlockFluidloggedTE)) old.renderBlockDamage(state, pos, texture, blockAccess);
        else old.renderBlockDamage(((BlockFluidloggedTE)state.getBlock()).getStored(blockAccess, pos), pos, texture, blockAccess);
    }

    @Override
    public void onResourceManagerReload(IResourceManager resourceManager) { old.onResourceManagerReload(resourceManager); }

    @Override
    public void renderBlockBrightness(IBlockState state, float brightness) { old.renderBlockBrightness(state, brightness); }

    @Override
    public IBakedModel getModelForState(IBlockState state) { return old.getModelForState(state); }

    @Override
    public BlockModelRenderer getBlockModelRenderer() { return old.getBlockModelRenderer(); }

    @Override
    public BlockModelShapes getBlockModelShapes() { return old.getBlockModelShapes(); }
}

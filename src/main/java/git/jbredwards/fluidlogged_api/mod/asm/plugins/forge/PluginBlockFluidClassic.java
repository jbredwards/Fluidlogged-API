package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.api.block.IFluidloggableFluid;
import git.jbredwards.fluidlogged_api.api.network.FluidloggedAPINetworkHandler;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.mod.common.config.FluidloggedAPIConfigHandler;
import git.jbredwards.fluidlogged_api.mod.common.message.MessageVaporizeEffects;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLiquid;
import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.fluids.*;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils.*;
import static net.minecraft.util.EnumFacing.*;

/**
 * modded fluids work properly with the mod
 * @author jbred
 *
 */
public final class PluginBlockFluidClassic implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("getOptimalFlowDirections"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * getOptimalFlowDirections: (changes are around line 217)
         * Old code:
         * {
         *     for (int side = 0; side < 4; side++)
         *     {
         *         ...
         *     }
         * ...
         * }
         * New code:
         * //creates an IBlockState local variable for later use (see usage in transformation below)
         * {
         *     IBlockState here = world.getBlockState(pos);
         *     for (int side = 0; side < 4; side++)
         *     {
         *         ...
         *     }
         * ...
         * }
         */
        if(instructions.getFirst() == insn.getPrevious()) {
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
            instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
            instructions.insertBefore(insn, new MethodInsnNode(INVOKEINTERFACE, "net/minecraft/world/IBlockAccess", obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", true));
            instructions.insertBefore(insn, new VarInsnNode(ASTORE, 5));
        }
        /*
         * getOptimalFlowDirections: (changes are around line 244)
         * Old code:
         * if (!canFlowInto(world, pos2) || isSourceBlock(world, pos2))
         * {
         *     continue;
         * }
         *
         * New code:
         * //ignore if the fluid can't flow from or flow into the given side
         * if (!canFlowInto(world, pos2) || Hooks.getOptimalFlowDirections(this, world, pos2, pos, here, side))
         * {
         *     continue;
         * }
         */
        else if(checkMethod(insn, "isSourceBlock")) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 2));
            list.add(new VarInsnNode(ALOAD, 5));
            list.add(new VarInsnNode(ILOAD, 3));
            list.add(genMethodNode("getOptimalFlowDirections", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;I)Z"));

            instructions.insert(insn, list);
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        /*
         * Implement IFluidloggableFluid, allows modded fluids that extend this class to be fluidloggable by default
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid");
        /*
         * getQuantaValue:
         * New code:
         * //allow this method to be able to get the quanta value from FluidStates
         * @Override
         * public int getQuantaValue(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.getQuantaValue(this, world, pos, quantaPerBlock);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("getQuantaValue"),
            "getQuantaValue", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
            }
        );
        /*
         * updateTick
         * New code:
         * //determines how this fluid behaves based on FluidStates
         * @Override
         * public void updateTick(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, @Nonnull Random rand)
         * {
         *     Hooks.fluidUpdateTick(this, world, pos, state, this.quantaPerBlock, this.densityDir, this.canCreateSources);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals(obfuscated ? "func_180650_b" : "updateTick"),
            "fluidUpdateTick", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;IIZ)V", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "quantaPerBlock", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z");
            }
        );
        /*
         * isFlowingVertically:
         * New code:
         * //check for FluidStates & canFluidFlow
         * public boolean isFlowingVertically(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.isFluidFlowingVertically(this, world, pos, this.densityDir);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("isFlowingVertically"),
            "isFluidFlowingVertically", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
            }
        );
        /*
         * canFlowInto:
         * New code:
         * //check for ICompatibleFluid
         * protected boolean canFlowInto(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.canFlowInto(this, world, pos);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("canFlowInto"),
            "canFlowInto", "(Lnet/minecraftforge/fluids/BlockFluidClassic;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
            }
        );
        /*
         * isSourceBlock:
         * New code:
         * //check for FluidStates & canFluidFlow
         * public boolean isSourceBlock(IBlockAccess world, BlockPos pos)
         * {
         *     return Hooks.isSourceBlock(this, world, pos, world.getBlockState(pos), null);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("isSourceBlock"),
            "isSourceBlock", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/util/EnumFacing;)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitMethodInsn(INVOKEINTERFACE, "net/minecraft/world/IBlockAccess", obfuscated ? "func_180495_p" : "getBlockState", "(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;", true);
                generator.visitInsn(ACONST_NULL);
            }
        );
        /*
         * place:
         * New code:
         * //allow the place method to fluidlog blocks
         * @Override
         * public int place(World world, BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace)
         * {
         *     return Hooks.place(this, world, pos, fluidStack, doPlace, this.getDefaultState());
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("place"),
            "place", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraftforge/fluids/FluidStack;ZLnet/minecraft/block/state/IBlockState;)I", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ILOAD, 4);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraft/block/Block", obfuscated ? "func_176223_P" : "getDefaultState", "()Lnet/minecraft/block/state/IBlockState;", false);
            }
        );
        /*
         * drain:
         * New code:
         * //allow the drain method to drain fluidlogged blocks
         * @Override
         * @Nullable
         * public FluidStack drain(World world, BlockPos pos, boolean doDrain)
         * {
         *     return Hooks.drain(this, world, pos, doDrain, this.stack);
         * }
         */
        overrideMethod(classNode, method -> method.name.equals("drain"),
            "drain", "(Lnet/minecraftforge/fluids/IFluidBlock;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;ZLnet/minecraftforge/fluids/FluidStack;)Lnet/minecraftforge/fluids/FluidStack;", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ILOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "stack", "Lnet/minecraftforge/fluids/FluidStack;");
            }
        );
        /*
         * isFluidloggableFluid:
         * New code:
         * //allow this fluid to be fluidloggable
         * @ASMGenerated
         * public boolean isFluidloggableFluid(@Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos)
         * {
         *     return Hooks.isFluidFluidloggable(this, fluid, world, pos, this.densityDir, this.canCreateSources);
         * }
         */
        addMethod(classNode, "isFluidloggableFluid", "(Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z",
            "isFluidFluidloggable", "(Lgit/jbredwards/fluidlogged_api/api/block/IFluidloggableFluid;Lnet/minecraft/block/state/IBlockState;Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;IZ)Z", generator -> {
                generator.visitVarInsn(ALOAD, 0);
                generator.visitVarInsn(ALOAD, 1);
                generator.visitVarInsn(ALOAD, 2);
                generator.visitVarInsn(ALOAD, 3);
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidBase", "densityDir", "I");
                generator.visitVarInsn(ALOAD, 0);
                generator.visitFieldInsn(GETFIELD, "net/minecraftforge/fluids/BlockFluidClassic", "canCreateSources", "Z");
            }
        );
        /*
         * isFluidloggableFluid:
         * New code:
         * //allow this fluid to be fluidloggable
         * @ASMGenerated
         * public boolean isFluidloggableFluid()
         * {
         *     return Hooks.isFluidFluidloggable(this);
         * }
         */
        addMethod(classNode, "isFluidloggableFluid", "()Z", "isFluidFluidloggable", "(Lnet/minecraft/block/Block;)Z",
                generator -> generator.visitVarInsn(ALOAD, 0));
        /*
         * =========
         * Accessors
         * =========
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/forge/PluginBlockFluidClassic$Accessor");
        /*
         * Accessor:
         * New code:
         * //add public getter for protected method
         * @ASMGenerated
         * public boolean[] getOptimalFlowDirections_Public(@Nonnull World world, @Nonnull BlockPos pos)
         * {
         *     return this.getOptimalFlowDirections(world, pos);
         * }
         */
        addMethod(classNode, "getOptimalFlowDirections_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "getOptimalFlowDirections", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)[Z", false);
        });
        /*
         * Accessor:
         * New code:
         * //add public getter for protected method
         * @ASMGenerated
         * public int getLargerQuanta_Public(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare)
         * {
         *     return this.getLargerQuanta(world, pos, compare);
         * }
         */
        addMethod(classNode, "getLargerQuanta_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "getLargerQuanta", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;I)I", false);
        });
        /*
         * Accessor:
         * New code:
         * //add public getter for protected method
         * @ASMGenerated
         * public boolean canFlowInto_Public(@Nonnull IBlockAccess world, @Nonnull BlockPos pos)
         * {
         *     return this.canFlowInto(world, pos);
         * }
         */
        addMethod(classNode, "canFlowInto_Public", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "canFlowInto", "(Lnet/minecraft/world/IBlockAccess;Lnet/minecraft/util/math/BlockPos;)Z", false);
        });
        /*
         * Accessor:
         * New code:
         * //add public getter for protected method
         * @ASMGenerated
         * public void flowIntoBlock_Public(@Nonnull World world, @Nonnull BlockPos pos, int meta)
         * {
         *     this.flowIntoBlock(world, pos, meta);
         * }
         */
        addMethod(classNode, "flowIntoBlock_Public", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitVarInsn(ALOAD, 2);
            generator.visitVarInsn(ILOAD, 3);
            generator.visitMethodInsn(INVOKEVIRTUAL, "net/minecraftforge/fluids/BlockFluidClassic", "flowIntoBlock", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;I)V", false);
        });

        return true;
    }

    //getOptimalFlowDirections, don't look up the state here each loop
    @Override
    public boolean addLocalVariables(@Nonnull MethodNode method, @Nonnull LabelNode start, @Nonnull LabelNode end, int index) {
        method.localVariables.add(new LocalVariableNode("here", "Lnet/minecraft/block/state/IBlockState;", null, start, end, 5));
        return true;
    }

    //the getOptimalFlowDirections transformer needs this
    @Override
    public boolean recalcFrames(boolean obfuscated) { return true; }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        @Nonnull
        public static final EnumFacing[] SIDES = { WEST, EAST, NORTH, SOUTH };

        public static boolean canFlowInto(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos) {
            final IBlockState state = world.getBlockState(pos);
            return isCompatibleFluid(getFluidFromState(state), block.getFluid()) && state.getValue(BlockLiquid.LEVEL) > 0 || block.canDisplace(world, pos);
        }

        @Nullable
        public static FluidStack drain(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, boolean doDrain, @Nullable FluidStack stack) {
            final IBlockState here = world.getBlockState(pos);
            final FluidState fluidState = getFluidState(world, pos, here);

            if(fluidState.isEmpty()) return null;
            if(doDrain) {
                if(fluidState.getState() == here) world.setBlockState(pos, Blocks.AIR.getDefaultState());
                else if(!setFluidState(world, pos, here, FluidState.EMPTY, false)) return null;
            }

            return fluidState.getLevel() != 0 ? null : stack == null ? new FluidStack(block.getFluid(), Fluid.BUCKET_VOLUME) : stack.copy();
        }

        public static void fluidUpdateTick(@Nonnull BlockFluidClassic block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull IBlockState state, int quantaPerBlock, int densityDir, boolean canCreateSources) {
            if(world.isRemote || !world.isAreaLoaded(pos, quantaPerBlock / 2)) return; // Forge: avoid loading unloaded chunks

            IBlockState here = world.getBlockState(pos); //fluidlogged fluids will have a different state here than the state input
            if(tryVaporizeHere(block.getFluid(), state, here, world, pos)) here = state; //try vaporize block here

            final EnumFacing facingDir = (densityDir > 0) ? UP : DOWN;
            int quantaRemaining = quantaPerBlock - state.getValue(BlockLiquid.LEVEL);

            // check adjacent block levels if non-source
            if(quantaRemaining < quantaPerBlock) {
                int adjacentSourceBlocks = 0;
                final int expQuanta;

                if(ForgeEventFactory.canCreateFluidSource(world, pos, state, canCreateSources)) {
                    for(EnumFacing facing : HORIZONTALS) {
                        BlockPos offset = pos.offset(facing);

                        if(isSourceBlock(block, world, offset, world.getBlockState(offset), facing.getOpposite()))
                            adjacentSourceBlocks++;
                    }
                }

                // new source block
                final IBlockState vertical = world.getBlockState(pos.up(densityDir));
                if(adjacentSourceBlocks >= 2 && (vertical.getMaterial().isSolid() || isSourceBlock(block, world, pos.up(densityDir), vertical, facingDir.getOpposite())))
                    expQuanta = quantaPerBlock;

                // vertical flow into block
                else if(PluginBlockFluidBase.Hooks.hasVerticalFlow(world, pos, block.getFluid(), densityDir)) expQuanta = quantaPerBlock - 1;

                else {
                    int maxQuanta = -100;
                    for(EnumFacing side : HORIZONTALS) {
                        BlockPos offset = pos.offset(side);

                        if(canFluidFlow(world, pos, here, side) && canFluidFlow(world, offset, world.getBlockState(offset), side.getOpposite()))
                            maxQuanta = ((Accessor)block).getLargerQuanta_Public(world, offset, maxQuanta);
                    }

                    expQuanta = maxQuanta - 1;
                }

                // decay calculation
                if(expQuanta != quantaRemaining) {
                    quantaRemaining = expQuanta;

                    if(expQuanta <= 0) world.setBlockToAir(pos);
                    else {
                        world.setBlockState(pos, state.withProperty(BlockLiquid.LEVEL, quantaPerBlock - expQuanta), Constants.BlockFlags.DEFAULT_AND_RERENDER);
                        world.scheduleUpdate(pos, block, block.tickRate(world));
                        world.notifyNeighborsRespectDebug(pos, block, false);
                    }
                }
            }

            //try flowing to nearby fluidloggable blocks
            else tryFlowIntoFluidloggable(block, world, pos, facingDir, state, here, quantaPerBlock, canCreateSources, HORIZONTALS);

            // Fluidlog vertical if possible
            if(FluidloggedAPIConfigHandler.verticalFluidloggedFluidSpread)
                tryFlowIntoFluidloggable(block, world, pos, facingDir, state, here, quantaPerBlock, canCreateSources, facingDir);

            // Flow vertically if possible
            if(canFluidFlow(world, pos, here, facingDir) && block.canDisplace(world, pos.up(densityDir))) {
                ((Accessor)block).flowIntoBlock_Public(world, pos.up(densityDir), 1);
                return;
            }

            // Flow outward if possible
            int flowMeta = quantaPerBlock - quantaRemaining + 1;
            if(flowMeta >= quantaPerBlock) return;

            if(isSourceBlock(block, world, pos, here, null) || !block.isFlowingVertically(world, pos)) {
                if(PluginBlockFluidBase.Hooks.hasVerticalFlow(world, pos, block.getFluid(), densityDir)) flowMeta = 1;

                final boolean[] flowTo = ((Accessor)block).getOptimalFlowDirections_Public(world, pos);
                for(int i = 0; i < 4; i++)
                    if(flowTo[i] && canFluidFlow(world, pos, here, SIDES[i]))
                        ((Accessor)block).flowIntoBlock_Public(world, pos.offset(SIDES[i]), flowMeta);
            }
        }

        //helper
        public static boolean tryVaporizeHere(@Nonnull Fluid fluid, @Nonnull IBlockState state, @Nonnull IBlockState here, @Nonnull World world, @Nonnull BlockPos pos) {
            if(FluidloggedAPIConfigHandler.lavalogVaporizeFlammable && here != state && state.getMaterial() == Material.LAVA) {
                //check game rule
                if(world.getGameRules().getBoolean("doFireTick")) {
                    boolean isFlammable = here.getMaterial().getCanBurn();
                    if(!isFlammable) {
                        for(EnumFacing facing : EnumFacing.values()) {
                            if(here.getBlock().isFlammable(world, pos, facing)) {
                                isFlammable = true;
                                break;
                            }
                        }
                    }

                    if(isFlammable) {
                        //certain blocks only call world.setBlockState on the server, like wooden signs for example
                        if(!world.isRemote) {
                            //handle clientside particles
                            FluidloggedAPINetworkHandler.INSTANCE.sendToAllAround(
                                    new MessageVaporizeEffects(fluid, pos),
                                    new NetworkRegistry.TargetPoint(
                                            world.provider.getDimension(),
                                            pos.getX() + 0.5,
                                            pos.getY() + 0.5,
                                            pos.getZ() + 0.5,
                                            64
                                    )
                            );

                            //handle serverside sound
                            fluid.vaporize(null, world, pos, new FluidStack(fluid, Fluid.BUCKET_VOLUME));
                        }

                        return world.setBlockState(pos, state);
                    }
                }
            }

            return false;
        }

        //helper
        public static void tryFlowIntoFluidloggable(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull EnumFacing facingDir, @Nonnull IBlockState state, @Nonnull IBlockState here, int quantaPerBlock, boolean canCreateSources, @Nonnull EnumFacing... flowInto) {
            if(quantaPerBlock > 0 && FluidloggedAPIConfigHandler.fluidloggedFluidSpread > 0 && (FluidloggedAPIConfigHandler.fluidloggedFluidSpread == 2 || state != here) && (state != here || isFluidloggableFluid(state, world, pos)) && ForgeEventFactory.canCreateFluidSource(world, pos, state, canCreateSources)) {
                for(EnumFacing facing : flowInto) {
                    if(canFluidFlow(world, pos, here, facing)) {
                        BlockPos offset = pos.offset(facing);
                        IBlockState neighbor = world.getBlockState(offset);

                        //check if the fluid could occupy the space
                        if(canFluidFlow(world, offset, neighbor, facing.getOpposite()) && isStateFluidloggable(neighbor, world, offset, block.getFluid()) && FluidState.get(world, offset).isEmpty()) {
                            //check for another source block that can flow into this
                            for(EnumFacing adjacentFacing : values()) {
                                if(adjacentFacing != facingDir && adjacentFacing != facing.getOpposite() && (adjacentFacing.getYOffset() == 0 || FluidloggedAPIConfigHandler.verticalFluidloggedFluidSpread) && canFluidFlow(world, offset, neighbor, adjacentFacing)) {
                                    BlockPos adjacentOffset = offset.offset(adjacentFacing);
                                    IBlockState adjacent = world.getBlockState(adjacentOffset);

                                    if(canFluidFlow(world, adjacentOffset, adjacent, adjacentFacing.getOpposite())) {
                                        //only allow certain FluidStates to count
                                        FluidState adjacentFluid = FluidloggedAPIConfigHandler.fluidloggedFluidSpread == 1
                                                ? FluidState.get(world, adjacentOffset)
                                                : getFluidState(world, adjacentOffset, adjacent);

                                        //set the FluidState in the world
                                        if(isCompatibleFluid(adjacentFluid.getFluid(), block.getFluid()) && isFluidloggableFluid(adjacentFluid.getState(), world, adjacentOffset)) {
                                            setFluidState(world, offset, neighbor, FluidState.of(block.getFluid()), false);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        public static boolean isFluidFlowingVertically(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int densityDir) {
            final EnumFacing facingDir = (densityDir < 0) ? UP : DOWN;

            final IBlockState here = world.getBlockState(pos);
            if(!canFluidFlow(world, pos, here, facingDir.getOpposite())) return false;

            final IBlockState neighbor = world.getBlockState(pos.up(densityDir));
            return isCompatibleFluid(getFluidState(world, pos.up(densityDir), neighbor).getFluid(), block.getFluid())
                    || (isCompatibleFluid(getFluidState(world, pos, here).getFluid(), block.getFluid())
                    && ((Accessor)block).canFlowInto_Public(world, pos.up(densityDir)));
        }

        public static boolean isFluidFluidloggable(@Nonnull IFluidloggableFluid block, @Nonnull IBlockState fluid, @Nonnull World world, @Nonnull BlockPos pos, int densityDir, boolean canCreateSources) {
            if(!block.isFluidloggableFluid()) return false;
            else if(fluid.getValue(BlockLiquid.LEVEL) == 0) return true;
            else if(!canCreateSources) return false;

            final IBlockState vertical = world.getBlockState(pos.down(densityDir));
            return isCompatibleFluid(block.getFluid(), getFluidState(world, pos.down(densityDir), vertical).getFluid())
                    && canFluidFlow(world, pos.down(densityDir), vertical, densityDir < 1 ? DOWN : UP);
        }

        public static boolean isFluidFluidloggable(@Nonnull Block block) {
            final @Nullable Fluid fluid = getFluidFromBlock(block);
            return fluid != null && block == fluid.getBlock() && !block.hasTileEntity();
        }

        public static boolean isSourceBlock(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, @Nonnull IBlockState here, @Nullable EnumFacing facing) {
            if(facing != null && !canFluidFlow(world, pos, here, facing)) return false;

            final FluidState fluidState = getFluidState(world, pos, here);
            return isCompatibleFluid(fluidState.getFluid(), block.getFluid()) && fluidState.getLevel() == 0;
        }

        public static boolean getOptimalFlowDirections(@Nonnull BlockFluidClassic block, @Nonnull IBlockAccess world, @Nonnull BlockPos offset, @Nonnull BlockPos pos, @Nonnull IBlockState here, int side) {
            return !canFluidFlow(world, pos, here, SIDES[side]) || block.isSourceBlock(world, offset);
        }

        public static int getQuantaValue(@Nonnull IFluidBlock block, @Nonnull IBlockAccess world, @Nonnull BlockPos pos, int quantaPerBlock) {
            final IBlockState here = world.getBlockState(pos);
            if(here.getBlock().isAir(here, world, pos)) return 0;

            final FluidState fluidState = getFluidState(world, pos, here);
            return isCompatibleFluid(fluidState.getFluid(), block.getFluid())
                    ? quantaPerBlock - fluidState.getLevel() : -1;
        }

        public static int place(@Nonnull IFluidBlock block, @Nonnull World world, @Nonnull BlockPos pos, @Nonnull FluidStack fluidStack, boolean doPlace, @Nonnull IBlockState defaultState) {
            if(fluidStack.amount < Fluid.BUCKET_VOLUME) return 0;
            if(doPlace) {
                final IBlockState here = world.getBlockState(pos);

                if(isStateFluidloggable(here, world, pos, block.getFluid())) {
                    if(!setFluidState(world, pos, here, FluidState.of(block.getFluid()), true))
                        return 0;
                }
                else {
                    FluidUtil.destroyBlockOnFluidPlacement(world, pos);
                    world.setBlockState(pos, defaultState, Constants.BlockFlags.DEFAULT_AND_RERENDER);
                }
            }

            return Fluid.BUCKET_VOLUME;
        }
    }

    public interface Accessor
    {
        @Nonnull
        boolean[] getOptimalFlowDirections_Public(@Nonnull World world, @Nonnull BlockPos pos);
        int getLargerQuanta_Public(@Nonnull IBlockAccess world, @Nonnull BlockPos pos, int compare);
        boolean canFlowInto_Public(@Nonnull IBlockAccess world, @Nonnull BlockPos pos);
        void flowIntoBlock_Public(@Nonnull World world, @Nonnull BlockPos pos, int meta);
    }
}

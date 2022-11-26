package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.world;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import git.jbredwards.fluidlogged_api.api.util.FluidState;
import git.jbredwards.fluidlogged_api.api.util.FluidloggedUtils;
import net.minecraft.block.Block;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.gen.structure.template.PlacementSettings;
import net.minecraft.world.gen.structure.template.Template;
import net.minecraftforge.common.util.Constants;
import org.apache.commons.lang3.tuple.Pair;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;

/**
 * structures can load saved FluidStates
 * @author jbred
 *
 */
public final class PluginTemplate implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(method.name.equals("<init>")) return 1;
        else if(method.name.equals(obfuscated ? "func_186254_a" : "takeBlocksFromWorld")) return 2;
        else if(checkMethod(method, obfuscated ? "func_189960_a" : "addBlocksToWorld", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/gen/structure/template/ITemplateProcessor;Lnet/minecraft/world/gen/structure/template/PlacementSettings;I)V"))
            return 3;
        else if(method.name.equals(obfuscated ? "func_189552_a" : "writeToNBT")) return 4;
        else if(method.name.equals(obfuscated ? "func_186256_b" : "read")) return 5;
        return 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * Constructor:
         * New code:
         * //initialize fluidStates list
         * {
         *     this.fluidStates = new ArrayList<>();
         *     ...
         * }
         */
        if(index == 1) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new TypeInsnNode(NEW, "java/util/ArrayList"));
            list.add(new InsnNode(DUP));
            list.add(new MethodInsnNode(INVOKESPECIAL, "java/util/ArrayList", "<init>", "()V", false));
            list.add(new FieldInsnNode(PUTFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
            instructions.insert(insn, list);
            return true;
        }
        //takeBlocksFromWorld
        else if(index == 2) {
            /*
             * takeBlocksFromWorld: (changes are around line 72)
             * Old code:
             * BlockPos blockpos = startPos.add(size).add(-1, -1, -1);
             *
             * New code:
             * //clear existing FluidState list before storing new
             * BlockPos blockpos = startPos.add(size).add(-1, -1, -1);
             * this.fluidStates.clear();
             */
            if(checkMethod(insn.getPrevious(), obfuscated ? "func_177982_a" : "add", "(III)Lnet/minecraft/util/math/BlockPos;")) {
                instructions.insert(insn, new MethodInsnNode(INVOKEINTERFACE, "java/util/List", "clear", "()V", true));
                instructions.insert(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
                instructions.insert(insn, new VarInsnNode(ALOAD, 0));
                return false;
            }
            /*
             * takeBlocksFromWorld: (changes are around line 87)
             * Old code:
             * TileEntity tileentity = worldIn.getTileEntity(blockpos$mutableblockpos);
             *
             * New code:
             * //gather FluidStates
             * TileEntity tileentity = worldIn.getTileEntity(blockpos$mutableblockpos);
             * Hooks.addFluidState(worldIn, blockpos$mutableblockpos, blockpos3, toIgnore, this.fluidStates);
             */
            else if(checkMethod(insn.getPrevious(), obfuscated ? "func_175625_s" : "getTileEntity")) {
                final InsnList list = new InsnList();
                list.add(new VarInsnNode(ALOAD, 1));
                list.add(new VarInsnNode(ALOAD, 13));
                list.add(new VarInsnNode(ALOAD, 14));
                list.add(new VarInsnNode(ALOAD, 5));
                list.add(new VarInsnNode(ALOAD, 0));
                list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
                list.add(genMethodNode("addFluidState", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/Block;Ljava/util/List;)V"));
                instructions.insert(insn, list);
                return true;
            }
        }
        else if(index == 3) {
            /*
             * addBlocksToWorld: (changes are around lines 281 & 285)
             * Old code:
             * if (worldIn.setBlockState(blockpos, iblockstate1, flags) && template$blockinfo1.tileentityData != null)
             * {
             *     ...
             * }
             *
             * New code:
             * //keep old fluids here
             * if (worldIn.setBlockState(blockpos, iblockstate1, Hooks.keepOldFlag(flags, this.keepOldFluidStates)) && template$blockinfo1.tileentityData != null)
             * {
             *     ...
             * }
             */
            if(checkMethod(insn, obfuscated ? "func_180501_a" : "setBlockState")) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z"));
                instructions.insertBefore(insn, genMethodNode("keepOldFlag", "(IZ)I"));
                return false;
            }
            /*
             * addBlocksToWorld: (changes are around line 330)
             * Old code:
             * {
             *     ...
             * }
             *
             * New code:
             * //place stored FluidStates
             * {
             *     ...
             *     Hooks.addFluidsToWorld(worldIn, pos, this.size, placementIn, flags, this.fluidStates);
             * }
             */
            else if(insn.getOpcode() == RETURN) {
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 1));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 2));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", obfuscated ? "field_186272_c" : "size", "Lnet/minecraft/util/math/BlockPos;"));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 4));
                instructions.insertBefore(insn, new VarInsnNode(ILOAD, 5));
                instructions.insertBefore(insn, new VarInsnNode(ALOAD, 0));
                instructions.insertBefore(insn, new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
                instructions.insertBefore(insn, genMethodNode("addFluidsToWorld", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/world/gen/structure/template/PlacementSettings;ILjava/util/List;)V"));
                return true;
            }
        }
        /*
         * writeToNBT: (Changes are around line 522)
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * //write FluidStates
         * {
         *     Hooks.writeTemplate(nbt, this.keepOldFluidStates, this.fluidStates);
         *     ...
         * }
         */
        if(index == 4) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z"));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
            list.add(genMethodNode("writeTemplate", "(Lnet/minecraft/nbt/NBTTagCompound;ZLjava/util/List;)V"));
            return true;
        }
        /*
         * writeToNBT: (Changes are around line 574)
         * Old code:
         * {
         *     ...
         * }
         *
         * New code:
         * //read FluidStates
         * {
         *     Hooks.readTemplate(this, nbt, this.fluidStates);
         *     ...
         * }
         */
        //read FluidStates
        if(index == 5) {
            final InsnList list = new InsnList();
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new VarInsnNode(ALOAD, 1));
            list.add(new VarInsnNode(ALOAD, 0));
            list.add(new FieldInsnNode(GETFIELD, "net/minecraft/world/gen/structure/template/Template", "fluidStates", "Ljava/util/List;"));
            list.add(genMethodNode("readTemplate", "(Lnet/minecraft/gen/structure/template/Template;Lnet/minecraft/nbt/NBTTagCompound;Ljava/util/List;)V"));
            return true;
        }

        return false;
    }

    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "keepOldFluidStates", "Z", null, null));
        classNode.fields.add(new FieldNode(ACC_PUBLIC | ACC_FINAL, "fluidStates", "Ljava/util/List;", "Ljava/util/List<Lorg/apache/commons/lang3/tuple/Pair<Lnet/minecraft/util/math/BlockPos;Lgit/jbredwards/fluidlogged_api/api/util/FluidState;>;>;", null));
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/plugins/vanilla/world/PluginTemplate$Accessor");
        /*
         * New code:
         * //allows the internal field to be set outside asm
         * @ASMGenerated
         * public void setKeepOldFluidStates(boolean keepOldFluidStates)
         * {
         *     this.keepOldFluidStates = keepOldFluidStates;
         * }
         */
        addMethod(classNode, "setKeepOldFluidStates", "(Z)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ILOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/world/gen/structure/template/Template", "keepOldFluidStates", "Z");
        });

        return true;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static void addFluidState(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull BlockPos transformedPos, @Nullable Block toIgnore, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
            final FluidState fluidState = FluidState.get(world, pos);
            if(!fluidState.isEmpty() && fluidState.getBlock() != toIgnore)
                fluidStates.add(Pair.of(transformedPos, fluidState));
        }

        public static void addFluidsToWorld(@Nonnull World world, @Nonnull BlockPos pos, @Nonnull BlockPos size, @Nonnull PlacementSettings settings, int flags, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
            if(!fluidStates.isEmpty() && size.getX() > 0 && size.getZ() > 0) {
                for(Pair<BlockPos, FluidState> entry : fluidStates) {
                    BlockPos transformedPos = Template.transformedBlockPos(settings, entry.getKey()).add(pos);
                    FluidloggedUtils.setFluidState(world, transformedPos, null, entry.getValue(), false, true, flags);
                }
            }
        }

        public static int keepOldFlag(int blockFlags, boolean keepOldFluidStates) { return keepOldFluidStates ? blockFlags : (blockFlags | 32); }

        public static void readTemplate(@Nonnull Template template, @Nonnull NBTTagCompound compound, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
            fluidStates.clear();

            if(compound.hasKey("fluidStates", Constants.NBT.TAG_LIST)) {
                for(NBTBase nbtBase : compound.getTagList("fluidStates", Constants.NBT.TAG_COMPOUND)) {
                    NBTTagCompound nbt = (NBTTagCompound)nbtBase;
                    FluidState fluidState = FluidState.of(Block.getBlockFromName(nbt.getString("state")));

                    if(!fluidState.isEmpty())
                        fluidStates.add(Pair.of(BlockPos.fromLong(nbt.getLong("pos")), fluidState));
                }
            }

            if(compound.hasKey("keepOldFluidStates", Constants.NBT.TAG_BYTE))
                ((Accessor)template).setKeepOldFluidStates(compound.getBoolean("keepOldFluidStates"));
        }

        public static void writeTemplate(@Nonnull NBTTagCompound compound, boolean keepOldFluidStates, @Nonnull List<Pair<BlockPos, FluidState>> fluidStates) {
            if(!fluidStates.isEmpty()) {
                final NBTTagList list = new NBTTagList();
                for(Pair<BlockPos, FluidState> entry : fluidStates) {
                    NBTTagCompound nbt = new NBTTagCompound();
                    nbt.setString("state", String.valueOf(entry.getValue().getBlock().getRegistryName()));
                    nbt.setLong("pos", entry.getKey().toLong());
                    list.appendTag(nbt);
                }

                compound.setTag("fluidStates", list);
            }

            compound.setBoolean("keepOldFluidStates", keepOldFluidStates);
        }
    }

    public interface Accessor
    {
        void setKeepOldFluidStates(boolean keepOldFluidStates);
    }
}

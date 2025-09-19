/*
 * Copyright (C) <2025 to Present> <jbredwards>
 *
 * All rights are reserved, except where explicitly granted by the original
 * copyright holder or where explicitly granted by the Mod Permissions License as
 * published by Jbredwards, either version 1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 * See the Mod Permissions License for more details
 * <https://www.github.com/jbredwards/mod-permissions-license>.
 */

package git.jbredwards.fluidlogged_api.mod.asm.plugins.vanilla.block;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * store one FluidState inside each BlockStateBase instance, this greatly increases the speed of fluid logic
 * @author jbred
 *
 */
public final class PluginBlockStateBase implements IASMPlugin
{
    @Override
    public boolean transformClass(@Nonnull ClassNode classNode, boolean obfuscated) {
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "blacklist", "Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;", null, null));
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/asm/iface/ICanFluidFlowHandler;", null, null));
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null));
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "fluidloggedAPI$boxes", "Ljava/util/List;", "Ljava/util/List<Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;>;", null));
        classNode.fields.add(new FieldNode(ACC_PUBLIC, "whitelist", "Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;", null, null));
        /*
         * =========
         * Accessors
         * =========
         */
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/ICanFluidFlowHandler$Accessor");
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IConfigAccessor");
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox");
        classNode.interfaces.add("git/jbredwards/fluidlogged_api/mod/asm/iface/IDefaultFluidState");
        /*
         * Accessor:
         * New code:
         * // getter for canFluidFlow
         * @ASMGenerated
         * public ICanFluidFlowHandler getCanFluidFlow()
         * {
         *     return this.canFluidFlow;
         * }
         */
        addMethod(classNode, "getCanFluidFlowOverride", "()Lgit/jbredwards/fluidlogged_api/mod/asm/iface/ICanFluidFlowHandler;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateBase", "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/asm/iface/ICanFluidFlowHandler;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for canFluidFlow
         * @ASMGenerated
         * public void setCanFluidFlow(ICanFluidFlowHandler canFluidFlow)
         * {
         *     this.canFluidFlow = canFluidFlow;
         * }
         */
        addMethod(classNode, "setCanFluidFlowOverride", "(Lgit/jbredwards/fluidlogged_api/mod/asm/iface/ICanFluidFlowHandler;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "canFluidFlow", "Lgit/jbredwards/fluidlogged_api/mod/asm/iface/ICanFluidFlowHandler;");
        });
        /*
         * Accessor:
         * New code:
         * // getter for defaultFluidState
         * @ASMGenerated
         * public FluidState getDefaultFluidState()
         * {
         *     return this.defaultFluidState;
         * }
         */
        addMethod(classNode, "getDefaultFluidState", "()Lgit/jbredwards/fluidlogged_api/api/util/FluidState;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateBase", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for defaultFluidState
         * @ASMGenerated
         * public void setDefaultFluidState(@Nonnull FluidState fluidState)
         * {
         *     this.defaultFluidState = fluidState;
         * }
         */
        addMethod(classNode, "setDefaultFluidState", "(Lgit/jbredwards/fluidlogged_api/api/util/FluidState;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "defaultFluidState", "Lgit/jbredwards/fluidlogged_api/api/util/FluidState;");
        });
        /*
         * Accessor:
         * New code:
         * // getter for blacklist
         * @ASMGenerated
         * public FluidState getBlacklistPredicate()
         * {
         *     return this.blacklist;
         * }
         */
        addMethod(classNode, "getBlacklistPredicate", "()Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateBase", "blacklist", "Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for blacklist
         * @ASMGenerated
         * public void setBlacklistPredicate(@Nullable ConfigPredicate predicate)
         * {
         *     this.blacklist = predicate;
         * }
         */
        addMethod(classNode, "setBlacklistPredicate", "(Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "blacklist", "Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;");
        });
        /*
         * Accessor:
         * New code:
         * // getter for whitelist
         * @ASMGenerated
         * public FluidState getWhitelistPredicate()
         * {
         *     return this.whitelist;
         * }
         */
        addMethod(classNode, "getWhitelistPredicate", "()Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateBase", "whitelist", "Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for whitelist
         * @ASMGenerated
         * public void setWhitelistPredicate(@Nullable ConfigPredicate predicate)
         * {
         *     this.whitelist = predicate;
         * }
         */
        addMethod(classNode, "setWhitelistPredicate", "(Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "whitelist", "Lgit/jbredwards/fluidlogged_api/mod/common/config/util/ConfigPredicate;");
        });
        /*
         * Accessor:
         * New code:
         * // getter for fluidloggedAPI$boxes
         * @ASMGenerated
         * public List<IConfigFluidBox.HeightBox> getBoxes()
         * {
         *     return this.fluidloggedAPI$boxes;
         * }
         */
        addMethod(classNode, "getBoxes", "()Ljava/util/List;", "()Ljava/util/List<Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;>;", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitFieldInsn(GETFIELD, "net/minecraft/block/state/BlockStateBase", "fluidloggedAPI$boxes", "Ljava/util/List;");
        });
        /*
         * Accessor:
         * New code:
         * // setter for fluidloggedAPI$boxes
         * @ASMGenerated
         * public void setBoxes(@Nullable List<IConfigFluidBox.HeightBox> boxes)
         * {
         *     this.fluidloggedAPI$boxes = boxes;
         * }
         */
        addMethod(classNode, "setBoxes", "(Ljava/util/List;)V", "(Ljava/util/List<Lgit/jbredwards/fluidlogged_api/mod/asm/iface/IConfigFluidBox$HeightBox;>;)V", null, null, generator -> {
            generator.visitVarInsn(ALOAD, 0);
            generator.visitVarInsn(ALOAD, 1);
            generator.visitFieldInsn(PUTFIELD, "net/minecraft/block/state/BlockStateBase", "fluidloggedAPI$boxes", "Ljava/util/List;");
        });

        return true;
    }
}

package git.jbredwards.fluidlogged_api.mod.asm.plugins.forge;

import git.jbredwards.fluidlogged_api.mod.asm.plugins.IASMPlugin;
import net.minecraft.util.EnumFacing;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * fixes all issues with fluidlogged z-fighting
 * @author jbred
 *
 */
public final class PluginModelFluid implements IASMPlugin
{
    @Override
    public int getMethodIndex(@Nonnull MethodNode method, boolean obfuscated) {
        if(checkMethod(method, "lambda$buildQuads$13", "(II)F")) return 1;
        else if(checkMethod(method, "lambda$buildQuads$11", "(II)F")) return 2;
        else return checkMethod(method, "lambda$buildQuads$7", "(I)F") ? 3 : 0;
    }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * buildQuads: (changes are around line 356)
         * Old code:
         * VertexParameter sideZ = j -> z[(si + x[j]) % 4];
         *
         * New code:
         * //offsets sides along the z-axis by 0.002 to fix fluidlogged z-fighting
         * VertexParameter sideZ = j -> Hooks.fixTextureFightingZ(z[(si + x[j]) % 4], si);
         */
        if(index == 1 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(genMethodNode("fixTextureFightingZ", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        /*
         * buildQuads: (changes are around line 354)
         * Old code:
         * VertexParameter sideX = j -> x[(si + x[j]) % 4];
         *
         * New code:
         * //offsets sides along the x-axis by 0.002 to fix fluidlogged z-fighting
         * VertexParameter sideX = j -> Hooks.fixTextureFightingX(x[(si + x[j]) % 4], si);
         */
        else if(index == 2 && insn.getOpcode() == I2F) {
            final InsnList list = new InsnList();
            //si local var
            list.add(new VarInsnNode(ILOAD, 0));
            //method
            list.add(genMethodNode("fixTextureFightingX", "(FI)F"));

            instructions.insert(insn, list);
            return true;
        }
        /*
         * buildQuads: (changes are around line 355)
         * Old code:
         * VertexParameter sideY = j -> z[j] == 0 ? (gas ? 1 : 0) : y[(si + x[j]) % 4];
         *
         * New code:
         * //offsets the bottom by 0.002 to fix fluidlogged z-fighting
         * VertexParameter sideY = j -> z[j] == 0 ? (gas ? 0.998f : 0.002f) : y[(si + x[j]) % 4];
         */
        else if(index == 3) {
            //gas
            if(insn.getOpcode() == FCONST_1) {
                instructions.insert(insn, new LdcInsnNode(0.998f));
                instructions.remove(insn);
            }
            //normal
            if(insn.getOpcode() == FCONST_0) {
                instructions.insert(insn, new LdcInsnNode(0.002f));
                instructions.remove(insn);
                return true;
            }
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static float fixTextureFightingX(float old, int index) {
            //[W, S, E, N]
            if(EnumFacing.byHorizontalIndex(5 - index).getDirectionVec().getX() == 0) return old;
            else return old == 1 ? 0.998f : 0.002f;
        }

        public static float fixTextureFightingZ(float old, int index) {
            //[W, S, E, N]
            if(EnumFacing.byHorizontalIndex(5 - index).getDirectionVec().getZ() == 0) return old;
            else return old == 1 ? 0.998f : 0.002f;
        }
    }
}

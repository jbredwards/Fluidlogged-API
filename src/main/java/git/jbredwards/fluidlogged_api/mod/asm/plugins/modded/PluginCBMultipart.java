package git.jbredwards.fluidlogged_api.mod.asm.plugins.modded;

import git.jbredwards.fluidlogged_api.api.asm.IASMPlugin;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.objectweb.asm.tree.*;

import javax.annotation.Nonnull;

/**
 * -allows multipart blocks to be placed in fluids
 * <p>
 * -prevents desync when removing a fluidlogged multipart block:
 * Server acts correctly and replaces the block in the world with the fluid.
 * Client, after receiving the block change from the server, calls world#setBlockToAir again, resulting in the client being set to air.
 * This fix removes the call on the client.
 *
 * @author jbred
 *
 */
public final class PluginCBMultipart implements IASMPlugin
{
    @Override
    public boolean isMethodValid(@Nonnull MethodNode method, boolean obfuscated) { return method.name.equals("place") || method.name.equals("remPart_do"); }

    @Override
    public boolean transform(@Nonnull InsnList instructions, @Nonnull MethodNode method, @Nonnull AbstractInsnNode insn, boolean obfuscated, int index) {
        /*
         * place:
         * Old code: (scala)
         * val hit = RayTracer.retrace(player)
         *
         * New code: (scala)
         * //don't stop the raytrace when it comes in contact with a fluid
         * val hit = RayTracer.retrace(player, false)
         */
        if(checkMethod(insn, "retrace")) {
            //change method to one with a flexible `stopAtFluid` flag & set it to false
            ((MethodInsnNode)insn).desc = "(Lnet/minecraft/entity/player/EntityPlayer;Z)Lnet/minecraft/util/math/RayTraceResult;";
            instructions.insertBefore(insn, new InsnNode(ICONST_0));
            return true;
        }
        /*
         * remPart_do:
         * Old code: (scala)
         * if (partList.isEmpty) world.setBlockToAir(pos)
         *
         * New code: (scala)
         * //prevent desync when removing a fluidlogged multipart block by only calling from the server
         * if (partList.isEmpty) Hooks.setBlockToAirServer(world, pos)
         */
        else if(checkMethod(insn, obfuscated ? "func_175698_g" : "setBlockToAir")) {
            instructions.insert(insn, genMethodNode("setBlockToAirServer", "(Lnet/minecraft/world/World;Lnet/minecraft/util/math/BlockPos;)Z"));
            instructions.remove(insn);
            return true;
        }

        return false;
    }

    @SuppressWarnings("unused")
    public static final class Hooks
    {
        public static boolean setBlockToAirServer(@Nonnull World world, @Nonnull BlockPos pos) {
            return !world.isRemote && world.setBlockToAir(pos);
        }
    }
}

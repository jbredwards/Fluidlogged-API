package git.jbredwards.fluidlogged_api.common.util;

import net.minecraft.block.BlockBush;
import net.minecraft.block.state.IBlockState;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import javax.annotation.Nonnull;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * makes some private & protected things accessible
 * @author jbred
 *
 */
public enum AccessorUtils
{
    ;
    //private or protected methods
    @Nonnull
    public static final Method canSustainBush = ObfuscationReflectionHelper.findMethod(BlockBush.class, "func_185514_i", boolean.class, IBlockState.class);
    static { canSustainBush.setAccessible(true); }

    public static boolean canSustainBush(@Nonnull BlockBush bush, @Nonnull IBlockState state) {
        try { return (boolean)canSustainBush.invoke(bush, state); }
        catch (InvocationTargetException | IllegalAccessException e) { return false; }
    }
}

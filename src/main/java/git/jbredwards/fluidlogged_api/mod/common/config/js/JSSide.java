package git.jbredwards.fluidlogged_api.mod.common.config.js;

import net.minecraft.util.EnumFacing;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.Immutable;

/**
 *
 * @author jbred
 *
 */
@Immutable
public enum JSSide
{
    DOWN(EnumFacing.DOWN),
    UP(EnumFacing.UP),
    NORTH(EnumFacing.NORTH),
    SOUTH(EnumFacing.SOUTH),
    WEST(EnumFacing.WEST),
    EAST(EnumFacing.EAST);

    @Nonnull final EnumFacing facing;
    JSSide(@Nonnull EnumFacing facingIn) { facing = facingIn; }

    @Nonnull
    public EnumFacing getJavaFacing() { return facing; }

    @Nonnull
    public String getAxis() { return facing.getAxis().getName(); }

    public int getXOffset() { return facing.getXOffset(); }

    public int getYOffset() { return facing.getYOffset(); }

    public int getZOffset() { return facing.getZOffset(); }

    public boolean isPositive() { return facing.getAxisDirection() == EnumFacing.AxisDirection.POSITIVE; }

    @Nonnull
    public JSSide getOpposite() { return fromFacing(facing.getOpposite()); }

    @Nonnull
    public static JSSide down() { return DOWN; }

    @Nonnull
    public static JSSide up() { return UP; }

    @Nonnull
    public static JSSide north() { return NORTH; }

    @Nonnull
    public static JSSide south() { return SOUTH; }

    @Nonnull
    public static JSSide west() { return WEST; }

    @Nonnull
    public static JSSide east() { return EAST; }

    @Nonnull
    public static JSSide of(float x, float y, float z) { return fromFacing(EnumFacing.getFacingFromVector(x, y, z)); }

    @Nonnull
    public static JSSide of(@Nullable Object o) {
        if(o instanceof JSSide) return (JSSide)o;
        else if(o instanceof EnumFacing) return fromFacing((EnumFacing)o);
        else if(o instanceof String) return fromFacing(EnumFacing.byName((String)o));
        else if(o instanceof Integer) return fromFacing(EnumFacing.byIndex((Integer)o));
        //oops
        else return EAST;
    }

    @Nonnull
    public static JSSide fromFacing(@Nullable EnumFacing facing) {
        if(facing == null) return EAST;
        switch(facing) {
            case DOWN: return DOWN;
            case UP: return UP;
            case NORTH: return NORTH;
            case SOUTH: return SOUTH;
            case WEST: return WEST;
            default: return EAST;
        }
    }
}

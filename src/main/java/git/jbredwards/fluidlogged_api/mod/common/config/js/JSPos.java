package git.jbredwards.fluidlogged_api.mod.common.config.js;

import net.minecraft.util.math.BlockPos;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.Immutable;

/**
 *
 * @author jbred
 *
 */
@Immutable
public final class JSPos
{
    @Nonnull final BlockPos pos;
    public JSPos(@Nonnull BlockPos posIn) { pos = posIn; }

    @Nonnull
    public static JSPos of(float x, float y, float z) { return new JSPos(new BlockPos(x, y, z)); }

    public int getX() { return pos.getX(); }
    public int getY() { return pos.getY(); }
    public int getZ() { return pos.getZ(); }

    @Nonnull
    public BlockPos getJavaPos() { return pos; }

    @Nonnull
    public JSPos add(int x, int y, int z) { return new JSPos(pos.add(x, y, z)); }

    @Nonnull
    public JSPos add(@Nonnull JSPos posIn) { return new JSPos(pos.add(posIn.pos)); }

    @Nonnull
    public JSPos subtract(@Nonnull JSPos posIn) { return new JSPos(pos.subtract(posIn.pos)); }

    @Nonnull
    public JSPos subtract(int x, int y, int z) { return add(-x, -y, -z); }

    @Nonnull
    public JSPos offset(@Nonnull JSSide side, int n) { return new JSPos(pos.offset(side.facing, n)); }

    @Nonnull
    public JSPos offset(@Nonnull JSSide side) { return offset(side, 1); }

    @Nonnull
    public JSPos up(int n) { return offset(JSSide.UP, n); }

    @Nonnull
    public JSPos up() { return up(1); }

    @Nonnull
    public JSPos down(int n) { return offset(JSSide.DOWN, n); }

    @Nonnull
    public JSPos down() { return down(1); }

    @Nonnull
    public JSPos north(int n) { return offset(JSSide.NORTH, n); }

    @Nonnull
    public JSPos north() { return north(1); }

    @Nonnull
    public JSPos south(int n) { return offset(JSSide.SOUTH, n); }

    @Nonnull
    public JSPos south() { return south(1); }

    @Nonnull
    public JSPos west(int n) { return offset(JSSide.WEST, n); }

    @Nonnull
    public JSPos west() { return west(1); }

    @Nonnull
    public JSPos east(int n) { return offset(JSSide.EAST, n); }

    @Nonnull
    public JSPos east() { return east(1); }

    @Override
    public boolean equals(@Nullable Object o) {
        if(o == null) return false;
        else if(this == o) return true;
        else return o instanceof JSPos && ((JSPos)o).pos.equals(pos);
    }
}

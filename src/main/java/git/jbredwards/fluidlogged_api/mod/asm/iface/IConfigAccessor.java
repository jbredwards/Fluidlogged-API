/*
 * Copyright (c) 2024-2025. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.asm.iface;

import git.jbredwards.fluidlogged_api.mod.common.config.util.ConfigPredicate;
import net.minecraft.block.state.BlockStateBase;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Set;

/**
 * Accessor implemented by {@link BlockStateBase} at runtime to allow for config-based fluidlogging interactions.
 * @author jbred
 *
 */
public interface IConfigAccessor
{
    @Nullable
    ConfigPredicate getBlacklistPredicate();
    void setBlacklistPredicate(@Nullable final ConfigPredicate predicate);

    @Nullable
    ConfigPredicate getWhitelistPredicate();
    void setWhitelistPredicate(@Nullable final ConfigPredicate predicate);

    /**
     * Stores all IBlockStates that have a blacklist config predicate entry, so they can be reset prior to any config reloads.
     */
    @Nonnull Set<IConfigAccessor> BLACKLIST_CACHE = new HashSet<>();
    static void setBlacklistAndCache(@Nonnull final IConfigAccessor state, @Nullable final ConfigPredicate predicate) {
        state.setBlacklistPredicate(predicate);
        if(predicate != null) BLACKLIST_CACHE.add(state);
    }

    /**
     * Stores all IBlockStates that have a whitelist config predicate entry, so they can be reset prior to any config reloads.
     */
    @Nonnull Set<IConfigAccessor> WHITELIST_CACHE = new HashSet<>();
    static void setWhitelistAndCache(@Nonnull final IConfigAccessor state, @Nullable final ConfigPredicate predicate) {
        state.setWhitelistPredicate(predicate);
        if(predicate != null) WHITELIST_CACHE.add(state);
    }
}

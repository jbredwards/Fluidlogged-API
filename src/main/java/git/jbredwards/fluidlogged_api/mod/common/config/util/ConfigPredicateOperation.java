/*
 * Copyright (c) 2024. jbredwards
 * All rights reserved.
 */

package git.jbredwards.fluidlogged_api.mod.common.config.util;

import com.google.gson.JsonParseException;

import javax.annotation.Nonnull;
import java.util.function.BiFunction;

/**
 *
 * @author jbred
 *
 */
public enum ConfigPredicateOperation implements BiFunction<ConfigPredicate, ConfigPredicate, ConfigPredicate>
{
    always {
        @Nonnull
        @Override
        public ConfigPredicate apply(@Nonnull final ConfigPredicate original, ConfigPredicate replacement) {
            return replacement;
        }
    },
    and {
        @Nonnull
        @Override
        public ConfigPredicate apply(@Nonnull final ConfigPredicate original, ConfigPredicate replacement) {
            return (world, pos, state, fluid) -> original.test(world, pos, state, fluid) && replacement.test(world, pos, state, fluid);
        }
    },
    never {
        @Nonnull
        @Override
        public ConfigPredicate apply(@Nonnull final ConfigPredicate original, ConfigPredicate replacement) {
            return original;
        }
    },
    or {
        @Nonnull
        @Override
        public ConfigPredicate apply(@Nonnull final ConfigPredicate original, ConfigPredicate replacement) {
            return (world, pos, state, fluid) -> original.test(world, pos, state, fluid) || replacement.test(world, pos, state, fluid);
        }
    };

    @Nonnull
    public static ConfigPredicateOperation get(@Nonnull final String nameIn) {
        for(@Nonnull final ConfigPredicateOperation op : values()) if(op.name().equals(nameIn)) return op;
        throw new JsonParseException("Could not get config predicate replacement operation from: \"" + nameIn + "\". Valid operations are: [always (default), and, never, or].");
    }
}

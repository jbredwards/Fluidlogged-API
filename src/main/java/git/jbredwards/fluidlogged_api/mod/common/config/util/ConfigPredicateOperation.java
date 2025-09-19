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

// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

/**
 * A configuration key (bare strings may also be used in place of a complete `Key`). Note that bare
 * strings are implicitly converted to non-private, description-free `Key`s.
 * @param name the name of the `Env` variable.
 * @param description an optional description, which will appear in `Trace` output.
 * @param secret true if the content of this `Env` variable should be redacted.
 */
case class Key(
  name:        String,
  description: Option[String],
  secret:      Boolean
)

object Key {

  implicit def stringToKey(s: String): Key =
    Key(s, None, false)

}

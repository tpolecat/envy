package envy
package config

/** Trait defining all config constructors. */
trait AllConfig
  extends NumericConfig
      with TextConfig

/** Module of all config constructors. */
object all extends AllConfig

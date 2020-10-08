package envy
package config

/** Trait defining text config constructors. */
trait TextConfig {

  /** Config that reads a `String` value. */
  def string(key: Key): Config[String] = { e =>
    val op = e(key.name)
    (Trace.Read(key, op), op)
  }

}

/** Module of numeric config constructors. */
object text extends TextConfig


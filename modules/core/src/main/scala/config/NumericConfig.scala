package envy
package config

/** Trait defining numeric config constructors. */
trait NumericConfig {
  import text.string

  private def numeric[A](key: Key, name: String, f: String => Option[A]): Config[A] =
    string(key).emap(s => f(s).toRight(s"$name: invalid value: $s"))

  def byte(key: String): Config[Byte] =
    numeric(key, "byte", _.toByteOption)

  def short(key: String): Config[Short] =
    numeric(key, "short", _.toShortOption)

  def int(key: String): Config[Int] =
    numeric(key, "int", _.toIntOption)

  def float(key: String): Config[Float] =
    numeric(key, "float", _.toFloatOption)

  def long(key: String): Config[Long] =
    numeric(key, "long", _.toLongOption)

  def double(key: String): Config[Double] =
    numeric(key, "double", _.toDoubleOption)

}

/** Module of numeric config constructors. */
object numeric extends NumericConfig


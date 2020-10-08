package envy

import cats.kernel.Monoid
import cats.effect.Sync

/** A partial mapping from `String` to `String`. */
trait Env {

  /**
   * Evaluate this `Env` at `s`.
   * @group Eliminators
   */
  def apply(s: String): Option[String]

}

/** Module of instances and consructors for `Env`. */
object Env {

  /**
   * `Env` is a monoid, where identity is the empty env and composition is "orElse".
   * @group Typeclass Instances
   */
  implicit val MonoidEnv: Monoid[Env] =
    new Monoid[Env] {
      def combine(x: Env, y: Env): Env = s => x(s) orElse y(s)
      def empty: Env = _ => None
    }

  /**
   * The empty environment.
   * @group Instances
   */
  val Empty: Env =
    MonoidEnv.empty

  /**
   * Construct an environment with the specified entries.
   * @group Constructors
   */
  def apply(entries: (String, String)*): Env =
    fromMap(entries.toMap)

  /**
   * Construct an environment that returns a constant value for every key.
   * @group Constructors
   */
  def const(s: String): Env =
    _ => Some(s)

  /**
   * Construct an environment that delegates to a `Map`. This is just a less-surprising alias for
   * `fromParialFunction`.
   * @group Constructors
   */
  def fromMap(map: Map[String, String]): Env =
    fromPartialFunction(map)

  /**
   * Construct an environment that delegates to a `PartialFunction`.
   * @group Constructors
   */
  def fromPartialFunction(pf: PartialFunction[String, String]): Env =
    pf.lift(_)

  /**
   * The environment that delegates to `scala.sys.env`.
   * @group Instances
   */
  val SysEnv: Env =
    fromMap(sys.env)

  /**
   * The environment that delegates to a constant snapshot of `scala.sys.props`. This value will be
   * observed only once, at an unspecified moment. If you wish to observe changes to Java properties
   * at runtime you should use the `fromJavaProps[F]` constructor instead.
   * @group Instances
   */
  val JavaProps: Env =
    fromMap(sys.props.toMap)

  /**
   * Computation that yields an `Env` that delegates to a snapshot of `scala.sys.props` (taken when
   * the action is executed). This allows your program to observe changes to Java properties at
   * runtime. If you don't need to do this use `JavaProps` instead.
   * @group Constructors
   */
  def fromJavaProps[F[_]: Sync]: F[Env] =
    Sync[F].delay(fromMap(sys.props.toMap))

}
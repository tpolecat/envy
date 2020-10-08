// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._
import cats.data._
import cats.implicits._

/** A configuration that attempts to map `Env` to `A`, tracing execution as it goes. */
trait Config[A] {

  /**
   * Apply an `Env` to this `Config`, yielding a trace and a possible result. `apply` is a
   * monoid homomorphism (up to tracing): `a(e1 |+| e2) <=> a(e1) <+> a(e2)`
   * @group Eliminators
   */
  def apply(env: Env): (Trace, Option[A])

  /**
   * Apply a transformation to the value computed by this `Config`, allowing failure.
   * @group Transformations
   */
  def emap[B](f: A => Either[String, B]): Config[B] = env =>
    apply(env) match {
      case (t, None)    => (t, None)
      case (t, Some(a)) =>
        f(a) match {
          case Left(msg) => (Trace.Invalid(msg, t), None)
          case Right(b)  => (t, Some(b))
        }
    }

  /**
   * Apply a transformation to the value computed by this `Config`.
   * @group Transformations
   */
  def map[B](f: A => B): Config[B] =
    Functor[Config].map(this)(f)

  /**
   * Compose with another `Config`, yielding a `Config` of a pair.
   * @group Combinators
   */
  def product[B](fb: Config[B]): Config[(A,B)] =
    (this, fb).parTupled

  /**
   * Compose sequentially with `f`.
   * @group Combinators
   */
  def flatMap[B](f: A => Config[B]): Config[B] =
    Monad[Config].flatMap(this)(f)

  /**
   * An equivalent `Config` that never fails, instead yielding an `Option`.
   * @group Combinators
   */
  def optional: Config[Option[A]] = { env =>
    val (t, op) = apply(env)
    (Trace.Optional(t, op.isDefined), Some(op).filter(_ => t.errors.nonEmpty))
  }

  // todo: default -- needs a new Trace constructor

}

/** Module of typeclass instances for `Config`. */
object Config {

  /**
   * `Config` is a monad with an error channel carrying a non-empty chain of strings.
   * @group Typeclass Instances
   */
  implicit val MonadErrorConfig: MonadError[Config, NonEmptyChain[String]] =
    new MonadError[Config, NonEmptyChain[String]] {

      def pure[A](a: A): Config[A] = _ =>
        (Trace.Pure, Some(a))

      def raiseError[A](messages: NonEmptyChain[String]): Config[A] = _ =>
        (messages.map[Trace](Trace.Error(_)).reduceLeft(_ && _), None)

      def handleErrorWith[A](fa: Config[A])(f: NonEmptyChain[String] => Config[A]): Config[A] = env =>
        fa(env) match {
          case (t, Some(a)) => (t, Some(a))
          case (t, None) =>
            NonEmptyChain.fromSeq(t.errors) match {
              case Some(nec) => f(nec)(env) // TODO: how do we not discard the prior trace? we need an "error handled" trace or something?
              case None      => (t, None)   // incomplete but no errors
            }
        }

      override def map[A, B](fa: Config[A])(f: A => B): Config[B] = { env =>
        val (t, a) = fa(env)
        (t, a.map(f))
      }

      def flatMap[A, B](fa: Config[A])(f: A => Config[B]): Config[B] = env =>
        fa(env) match {
          case (t, None)    => (t && Trace.Halt, None)
          case (t, Some(a)) =>
            f(a)(env) match {
              case (tʹ, b) => (t && tʹ, b)
            }
        }

      def tailRecM[A, B](a: A)(f: A => Config[Either[A,B]]): Config[B] = { env =>

        def go(t: Trace, op: Option[Either[A, B]]): (Trace, Option[B]) =
          op match {
            case None           => (t && Trace.Halt, None)
            case Some(Right(b)) => (t, Some(b))
            case Some(Left(a))  =>
              val (tʹ, op) = f(a)(env)
              go(t && tʹ, op)
          }

        val (t, op) = f(a)(env)
        go(t, op)

      }

    }

  /**
   * `Config` is a universally quantified semigroup.
   * @group Typeclass Instances
   */
  implicit val SemgroupKConfig: SemigroupK[Config] =
    new SemigroupK[Config] {
      def combineK[A](x: Config[A], y: Config[A]): Config[A] = { env =>
        val (tx, ax) = x(env)
        val (ty, ay) = y(env)
        (tx || ty, ax <+> ay)
      }
    }

  /**
   * `Config` is associated with parallel applicative functor `Config.Par`.
   * @group Typeclass Instances
   */
  implicit val ParallelEnv: Parallel[Config] =
    new Parallel[Config] {
      type F[A] = Par[A]
      val sequential  = new (Par ~> Config) { def apply[A](fa: Par[A]) = fa(_) }
      val parallel    = new (Config ~> Par) { def apply[A](fa: Config[A]) = fa(_) }
      val applicative = Par.ApplicativePar
      val monad       = Config.MonadErrorConfig
   }

  /**
   * Isomorphic newtype for `Config` with parallel applicative composition.
   * @group Parallel Applicative
   */
  trait Par[A] {
    def apply(env: Env): (Trace, Option[A])
  }

  /**
   * Module of typeclass instances for `Par`.
   * @group Parallel Applicative
   */
  object Par {

    /**
     * `Par` is an applicative functor.
     * @group Typeclass Instances
     */
    implicit val ApplicativePar: Applicative[Par] =
      new Applicative[Par] {

        def pure[A](a: A): Par[A] = _ =>
          (Trace.Pure, Some(a))

        def ap[A, B](fab: Par[A => B])(fa: Par[A]): Par[B] = { env =>
          val (t1, f) = fab(env)
          val (t2, a) = fa(env)
          (t1 && t2, f ap a)
        }

      }

  }


}


// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._
import cats.implicits._
import cats.effect.Sync

sealed abstract case class Env[A](run: (String => Option[String]) => (Trace, Option[Key], Option[A])) { outer =>

  def load[F[_]: Sync]: F[A] =
    Sync[F].delay(run((sys.env orElse sys.props).lift)).flatMap {
      case (_, _, Some(a)) => a.pure[F]
      case (t, _, None)    => Sync[F].raiseError(new RuntimeException(
        s"""|Configuration failed.
            |
            |Configuration has failed due to missing or invalid environment variables and/or system
            |properties. The configuration specification below indicates what is required, what has
            |been provided thus far [x] and what is invalid [!]. If the configuration includes
            |sequential (monadic) composition then some of the configuration may remain unspecified
            |until sequential execution can continue.
            |
            |${t.describe("  ")}
            |""".stripMargin
      ))
    }

  def pureLoad(env: String => Option[String]): (Trace, Option[A]) =
    run(env) match {
      case (t, _, a) => (t, a)
    }

  def emap[B](f: A => Either[String, B]): Env[B] =
    Env { env =>
      run(env) match {
        case (t, k, None) => (t, k, None)
        case (t, k, Some(a)) =>
          f(a) match {
            case Left(msg) => (Trace.Failure(k, Some(t), msg), k, None)
            case Right(b)  => (t, k, Some(b))
          }
      }
    }

  def option: Env[Option[A]] =
    optional

  def optional: Env[Option[A]] =
    Env { env =>
      run(env) match {
        case (t, k, op) => (Trace.Optional(t, op.isDefined), k, Some(op).filter(_ => t.error.isDefined))
      }
    }

  def to[B](implicit ev: EMapper[A, B]): Env[B] =
    emap(ev.apply)

  // TODO: remove
  def as[B](implicit ev: EMapper[A, B]): Env[B] =
    to[B]

  def or(that: Env[A]): Env[A] =
    this <+> that

  def default(value: A): Env[A] =
    Env { env =>
      val (t, k, a) = outer.run(env)
      if (t.error.isDefined) (t, k, a) else (t, k, a orElse Some(value))
    }

  def redacted = this

}

object Env {

  private[envy] def apply[A](run: (String => Option[String]) => (Trace, Option[Key], Option[A])) =
    new Env[A](run) {}

  implicit val MonadErrorEnv: MonadError[Env, String] with SemigroupK[Env] =
    new MonadError[Env, String] with SemigroupK[Env] {

      def pure[A](a: A): Env[A] =
        Env(_ => (Trace.Pure, None, Some(a)))

      def raiseError[A](error: String): Env[A] =
        Env(_ => (Trace.Failure(None, None, error), None, None))

      def handleErrorWith[A](fa: Env[A])(f: String => Env[A]): Env[A] =
        Env { env =>
          val (t, k, a) = fa.run(env)
          t.error match {
            case None => (t, k, a)
            case Some(e) =>
              val (tʹ, kʹ, aʹ) = f(e).run(env)
              (t && tʹ, kʹ <+> k, aʹ)
          }
        }

      override def map[A, B](fa: Env[A])(f: A => B): Env[B] =
        Env { env =>
          fa.run(env) match {
            case (t, k, a) => (t, k, a.map(f))
          }
        }

      def flatMap[A, B](fa: Env[A])(f: A => Env[B]): Env[B] =
        Env { env =>
          fa.run(env) match {
            case (t, k, None)    => (t && Trace.Halt, k, None)
            case (t, k, Some(a)) =>
              f(a).run(env) match {
                case (tʹ, kʹ, b) => (t && tʹ, kʹ <+> k, b)
              }
          }
        }

      def tailRecM[A, B](a: A)(f: A => Env[Either[A,B]]): Env[B] =
        Env { env =>

          def go(t: Trace, k: Option[Key], op: Option[Either[A, B]]): (Trace, Option[Key], Option[B]) =
            op match {
              case None           => (t && Trace.Halt, k, None)
              case Some(Right(b)) => (t, k, Some(b))
              case Some(Left(a))  =>
                val (tʹ, kʹ, op) = f(a).run(env)
                go(t && tʹ, kʹ <+> k, op)
            }

          val (t, k, op) = f(a).run(env)
          go(t, k, op)

        }

      def combineK[A](x: Env[A], y: Env[A]): Env[A] =
        Env { env =>
          val (tx, _, ax) = x.run(env)
          val (ty, _, ay) = y.run(env)
          (tx || ty, None, ax <+> ay)
        }

    }

  implicit val ParallelEnv: Parallel[Env] =
    new Parallel[Env] {
      type F[A] = ParEnv[A]
      val sequential  = new (ParEnv ~> Env) { def apply[A](fa: ParEnv[A]) = Env(fa.run) }
      val parallel    = new (Env ~> ParEnv) { def apply[A](fa: Env[A]) = ParEnv(fa.run) }
      val applicative = ParEnv.ApplicativeParEnv
      val monad       = Env.MonadErrorEnv
   }

}


// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._
import cats.data._
import cats.implicits._
// import cats.effect.Sync

sealed abstract case class Config[A](run: (String => Option[String]) => (Trace, Option[A])) { outer =>

  // def load[F[_]: Sync]: F[A] =
  //   Sync[F].delay(run((sys.env orElse sys.props).lift)).flatMap {
  //     case (_, _, Some(a)) => a.pure[F]
  //     case (t, _, None)    => Sync[F].raiseError(new RuntimeException(
  //       s"""|Configuration failed.
  //           |
  //           |Configuration has failed due to missing or invalid environment variables and/or system
  //           |properties. The configuration specification below indicates what is required, what has
  //           |been provided thus far [x] and what is invalid [!]. If the configuration includes
  //           |sequential (monadic) composition then some of the configuration may remain unspecified
  //           |until sequential execution can continue.
  //           |
  //           |${t.describe("  ")}
  //           |""".stripMargin
  //     ))
  //   }

  def emap[B](f: A => Either[String, B]): Config[B] =
    Config { env =>
      run(env) match {
        case (t, None)    => (t, None)
        case (t, Some(a)) =>
          f(a) match {
            case Left(msg) => (Trace.Invalid(msg, t), None)
            case Right(b)  => (t, Some(b))
          }
      }
    }

  def option: Config[Option[A]] =
    optional

  def optional: Config[Option[A]] =
    Config { env =>
      val (t, op) = run(env)
      (Trace.Optional(t, op.isDefined), Some(op).filter(_ => t.errors.nonEmpty))
    }

  def to[B](implicit ev: EMapper[A, B]): Config[B] =
    emap(ev.apply)

  // TODO: remove
  def as[B](implicit ev: EMapper[A, B]): Config[B] =
    to[B]

  def or(that: Config[A]): Config[A] =
    this <+> that

  // def default(value: A): Env[A] =
  //   Env { env =>
  //     val (t, k, a) = outer.run(env)
  //     if (t.error.isDefined) (t, k, a) else (t, k, a orElse Some(value))
  //   }

  def redacted = this

}

object Config {

  private[envy] def apply[A](run: (String => Option[String]) => (Trace, Option[A])) =
    new Config[A](run) {}

  implicit val MonadErrorEnv: MonadError[Config, NonEmptyChain[String]] with SemigroupK[Config] =
    new MonadError[Config, NonEmptyChain[String]] with SemigroupK[Config] {

      def pure[A](a: A): Config[A] =
        Config(_ => (Trace.Pure, Some(a)))

      def raiseError[A](messages: NonEmptyChain[String]): Config[A] =
        Config { _ =>
          (messages.map[Trace](Trace.Error(_)).reduceLeft(_ && _), None)
        }

      def handleErrorWith[A](fa: Config[A])(f:  NonEmptyChain[String] => Config[A]): Config[A] =
        Config { env =>
          fa.run(env) match {
            case (t, Some(a)) => (t, Some(a))
            case (t, None) =>
              NonEmptyChain.fromSeq(t.errors) match {
                case Some(nec) => f(nec).run(env) // TODO: how do we not discard the prior trace? we need an "error handled" trace or something?
                case None      => (t, None)       // incomplete but no errors
              }
          }
        }

      override def map[A, B](fa: Config[A])(f: A => B): Config[B] =
        Config { env =>
          val (t, a) = fa.run(env)
          (t, a.map(f))
        }

      def flatMap[A, B](fa: Config[A])(f: A => Config[B]): Config[B] =
        Config { env =>
          fa.run(env) match {
            case (t, None)    => (t && Trace.Halt, None)
            case (t, Some(a)) =>
              f(a).run(env) match {
                case (tʹ, b) => (t && tʹ, b)
              }
          }
        }

      def tailRecM[A, B](a: A)(f: A => Config[Either[A,B]]): Config[B] =
        Config { env =>

          def go(t: Trace, op: Option[Either[A, B]]): (Trace, Option[B]) =
            op match {
              case None           => (t && Trace.Halt, None)
              case Some(Right(b)) => (t, Some(b))
              case Some(Left(a))  =>
                val (tʹ, op) = f(a).run(env)
                go(t && tʹ, op)
            }

          val (t, op) = f(a).run(env)
          go(t, op)

        }

      def combineK[A](x: Config[A], y: Config[A]): Config[A] =
        Config { env =>
          val (tx, ax) = x.run(env)
          val (ty, ay) = y.run(env)
          (tx || ty, ax <+> ay)
        }

    }

  implicit val ParallelEnv: Parallel[Config] =
    new Parallel[Config] {
      type F[A] = Par[A]
      val sequential  = new (Par ~> Config) { def apply[A](fa: Par[A]) = Config(fa.run) }
      val parallel    = new (Config ~> Par) { def apply[A](fa: Config[A]) = Par(fa.run) }
      val applicative = Par.ApplicativeParEnv
      val monad       = Config.MonadErrorEnv
   }


  sealed abstract case class Par[A](
    run: (String => Option[String]) => (Trace, Option[A])
  )

  object Par {

    private[envy] def apply[A](run: (String => Option[String]) => (Trace, Option[A])) =
      new Par[A](run) {}

    implicit val ApplicativeParEnv: Applicative[Par] =
      new Applicative[Par] {

        def pure[A](a: A): Par[A] =
          Par(_ => (Trace.Pure, Some(a)))

        def ap[A, B](fab: Par[A => B])(fa: Par[A]): Par[B] =
          Par { env =>
            val (t1, f) = fab.run(env)
            val (t2, a) = fa.run(env)
            (t1 && t2, f ap a)
          }
      }

  }


}


// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._
import cats.implicits._

sealed abstract case class ParEnv[A](
  run: (String => Option[String]) => (Trace, Option[Key], Option[A])
)

object ParEnv {

  private[envy] def apply[A](run: (String => Option[String]) => (Trace, Option[Key], Option[A])) =
    new ParEnv[A](run) {}

  implicit val ApplicativeParEnv: Applicative[ParEnv] =
    new Applicative[ParEnv] {

      def pure[A](a: A): ParEnv[A] =
        ParEnv(_ => (Trace.Pure, None, Some(a)))

      def ap[A, B](fab: ParEnv[A => B])(fa: ParEnv[A]): ParEnv[B] =
        ParEnv { env =>
          val (t1, _, f) = fab.run(env)
          val (t2, _, a) = fa.run(env)
          (t1 && t2, None, f ap a)
        }
    }

}


// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._

// This is ALSO a monaderror+parallel just like Env but it feels weird to implement all that stuff
// again. But it allows us to distinguish `a.run(e1 <+> e2)` vs. `a.run(e1) <+> a.run(e2)`.

sealed trait Result[A] {
  import Result._

  def trace: Trace

  def toEither: Either[Trace, A] =
    this match {
      case Complete(_, a) => Right(a)
      case r              => Left(r.trace)
    }

  def toOption: Option[A] =
    Some(this).collect { case Complete(_, a) => a }

  def getF[F[_]](
    implicit ev: ApplicativeError[F, Throwable]
  ): F[A] =
    this match {
      case Complete(_, a) => ev.pure(a)
      case r              => ev.raiseError(r.trace.toException)
    }

}

object Result {

  case class Complete[A](trace: Trace, value: A) extends Result[A]
  case class Incomplete[A](trace: Trace)         extends Result[A]
  case class Invalid[A](trace: Trace)            extends Result[A]

}



// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._

// This is ALSO a monaderror+parallel just like Env but it feels weird to implement all that stuff
// again. But it allows us to distinguish `a.run(e1 <+> e2)` vs. `a.run(e1) <+> a.run(e2)`.

/** ADT for results of a `Config` execution. */
sealed trait Result[A] {
  import Result._

  /**
   * A `Trace` describing the execition that resulted in this `Result`.
   * @group Properties
   */
  def trace: Trace

  /**
   * Convert this `Trace` to an `Either`, discarding traces for successful results, as well as the
   * distinction between failure cases.
   * @group Eliminators
   */
  def toEither: Either[Trace, A] =
    this match {
      case Complete(_, a) => Right(a)
      case r              => Left(r.trace)
    }

  /**
   * Convert this `Trace` to an `Option`, discarding traces and the distinction between failure
   * cases.
   * @group Eliminators
   */
  def toOption: Option[A] =
    Some(this).collect { case Complete(_, a) => a }

  /**
   * Extract the result if possible, otherwise raise an error in `F`.
   * @group Eliminators
   */
  def getF[F[_]](
    implicit ev: ApplicativeError[F, Throwable]
  ): F[A] =
    this match {
      case Complete(_, a) => ev.pure(a)
      case r              => ev.raiseError(new RuntimeException(r.trace.toString())) // todo
    }

}

object Result {

  /**
   * A successful configuration result.
   * @group Constructors
   */
  case class Complete[A](trace: Trace, value: A) extends Result[A]

  /**
   * A failed configuration result, due to missing `Env` values.
   * @group Constructors
   */
  case class Incomplete[A](trace: Trace) extends Result[A]

  /**
   * A failed configuration result, due to invalid `Env` values.
   * @group Constructors
   */
  case class Invalid[A](trace: Trace) extends Result[A]

}



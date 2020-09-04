// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats.implicits._
import scala.reflect.ClassTag
import cats.NotNull
import java.util.UUID

trait EMapper[A, B] { outer =>

  def apply(a: A): Either[String, B]

  def collect[C](name: String)(pf: PartialFunction[B, C]): EMapper[A, C] =
    mapOption(name)(pf.lift)

  def map[C](f: B => C): EMapper[A, C] =
    new EMapper[A, C] {
      def apply(a: A): Either[String,C] =
        outer.apply(a).map(f)
    }

  def mapOption[C](name: String)(f: B => Option[C]): EMapper[A, C] =
    new EMapper[A, C] {
      def apply(a: A): Either[String, C] =
        outer.apply(a).flatMap { b =>
          f(b).toRight(s"invalid $name: $b")
        }
    }

  object mapCatchOnly {
    def apply[T >: Null <: Throwable](name: String) = new Partial[T](name)
    class Partial[T >: Null <: Throwable](name: String) {
      def apply[C](f: B => C)(implicit CT: ClassTag[T], NT: NotNull[T]): EMapper[A, C] =
        mapOption(name)(b => Either.catchOnly[T](f(b)).toOption)
    }
  }

}

object EMapper {

  final def apply[A]: EMapper[A, A] =
    _.asRight

  implicit val StringInt: EMapper[String, Int] =
    EMapper[String].mapCatchOnly[NumberFormatException]("int")(_.toInt)

  implicit val StringUUID: EMapper[String, UUID] =
    EMapper[String].mapCatchOnly[IllegalArgumentException]("uuid")(UUID.fromString)

}

// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats.data._
import cats._
import cats.implicits._

sealed trait Trace {
  import Trace._

  def failed: Boolean = incomplete || error.isDefined

  /** True if the configuration is incomplete due to missing keys. */
  def incomplete: Boolean   = false

  def error: Option[String] = None // Chain[String]

  /** Product of traces; this and that. */
  def &&(that: Trace): Trace = Conj(this, that)

  /** Coproduct of traces; this or that. */
  def ||(that: Trace): Trace = Disj(this, that)

  def icons: String =
    if (error.isDefined) "[!]"
    else if (incomplete) "[ ]"
    else "[x]"

  def describe(indent: String): String =
    s"$indent$icons $describeImpl"

  def describeImpl: String

  def dump(): Unit =
    println(describe(""))

}

trait CompositeTrace extends Trace {
  def elems: NonEmptyChain[Trace]
  override def error: Option[String] = elems.map(_.error).collectFirst { case Some(s) => s }
  override def describe(indent: String) = s"$indent$icons $describeImpl\n${elems.map(_.describe(indent + "    ")).intercalate("\n")}"
}

object Trace {

  case object Pure extends Trace {
    def describeImpl = s"<pure>"
  }

  case object Halt extends Trace {
    override def icons: String = "[-]"
    def describeImpl: String = "<sequential execution halted>"
  }

  case class Success(key: Key) extends Trace {
    def describeImpl: String = s"${key.name}${key.description.foldMap(d => s": $d")}"
  }
  case class Missing(key: Key) extends Trace {
    override def incomplete: Boolean = true
    def describeImpl: String = s"${key.name}${key.description.foldMap(d => s": $d")}"
  }
  case class Failure(key: Option[Key], prior: Option[Trace], message: String) extends Trace {
    override def error: Option[String] = Some(message)
    override def incomplete: Boolean = prior.exists(_.incomplete)
    def describeImpl: String = s"${key.foldMap(key => s"${key.name}: ")}$message"
  }
  case class Optional(trace: Trace, provided: Boolean) extends CompositeTrace {
    override def error: Option[String] = trace.error
    def status = if (error.isDefined) "" else s" (${if (provided) "provided" else "not provided"})"
    def describeImpl: String = s"optional$status"
    def elems: NonEmptyChain[Trace] = NonEmptyChain(trace)
  }

  case class Conj(elems: NonEmptyChain[Trace]) extends CompositeTrace {
    override def incomplete: Boolean = elems.exists(_.incomplete)
    def describeImpl: String = "all of the following"
    override def &&(other: Trace): Trace =
      other match {
        case Conj(b) => Conj(elems :++ b.filterNot(elems.contains))
        case b       => this && Conj(b)
      }
  }
  object Conj {
    def apply(a: Trace, as: Trace*): Conj =
      Conj(NonEmptyChain(a, as: _*))
  }

  case class Disj(elems: NonEmptyChain[Trace]) extends CompositeTrace {
    override def error: Option[String] = elems.map(_.error).collectFirst { case Some(s) => s }
    override def incomplete: Boolean = elems.forall(_.incomplete)
    def describeImpl: String = "one of the following"
    override def ||(other: Trace): Trace =
      other match {
        case Disj(b) => Disj(elems :++ b.filterNot(elems.contains))
        case b       => this || Disj(b)
      }
  }
  object Disj {
    def apply(a: Trace, as: Trace*): Disj =
      Disj(NonEmptyChain(a, as: _*))
  }

  implicit val EqTrace: Eq[Trace] =
    Eq.fromUniversalEquals

}


///


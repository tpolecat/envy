// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

import cats._
import cats.data._
import cats.implicits._

/** ADT that represents traces of `Config` execution. */
sealed trait Trace {
  import Trace._

  /**
   * A descriptive title for this `Trace`.
   * @group Properties
   */
  def title:      String

  /**
   * True if missing values have prevented completion of the `Config` evaluation. Note that
   * completion and error accumulation are orthogonal.
   * @group Properties
   */
  def incomplete: Boolean      = false

  /**
   * Accumulated errors (if any). Note that completion and error accumulation are orthogonal.
   * @group Properties
   */
  def errors:     List[String] = Nil

  /**
   * Construct a composite trace of this AND that.
   * @group Combinators
   */
  def &&(that: Trace): Trace =
    (this, that) match {

      // pure gets absorbed
      case (a, Pure) => a
      case (Pure, a) => a

      // conj gets joined
      case (Conjunction(a), Conjunction(b)) => Conjunction(a :++ b.filterNot(a.contains))
      case (Conjunction(a), b)              => Conjunction(a) && Conjunction(b)
      case (a, Conjunction(b))              => Conjunction(a) && Conjunction(b)

      // Otherwise, new conjunction
      case (a, b) => Conjunction(a, b)

    }

  /**
   * Construct a composite trace of this OR that.
   * @group Combinators
   */
  def ||(that: Trace): Trace =
    (this, that) match {

      // pure gets absorbed
      case (a, Pure) => a
      case (Pure, a) => a

      // disj gets joined
      case (Disjunction(a), Disjunction(b)) => Disjunction(a :++ b.filterNot(a.contains))
      case (Disjunction(a), b)              => Disjunction(a) || Disjunction(b)
      case (a, Disjunction(b))              => Disjunction(a) || Disjunction(b)

      // Otherwise, new disjunction
      case (a, b) => Disjunction(a, b)

    }

  /**
   * This `Trace` as a multi-line tree, suitable for debugging or error reporting.
   * @group Properties
   */
  final override def toString(): String =
    toTree(0)

  private def icon: String =
         if (this == Trace.Halt) "[-]" // special case
    else if (errors.nonEmpty)    "[!]" // error
    else if (incomplete)         "[ ]" // incomplete
    else                         "[+]" // all good!

  protected def toTree(indent: Int): String =
    s"${" " * (indent * 4)}$icon $title"

}

object Trace {

  /**
   * `Trace` equality is defined structurally.
   * @group Typeclass Instances
   */
  implicit lazy val EqTrace: Eq[Trace] =
    Eq.fromUniversalEquals

  /**
   * A trace that aggregates one or more prior traces.
   * @group Utility Classes
   */
  sealed abstract class CompositeTrace(elements: NonEmptyChain[Trace]) extends Trace {
    override def incomplete = elements.exists(_.incomplete)
    override def errors     = elements.foldMap(_.errors)
    protected override def toTree(indent: Int): String =
      (super.toTree(indent) +: elements.map(_.toTree(indent + 1))).intercalate("\n")
  }

  /**
   * A trace indicating a pure value was yielded. These proliferate via appliciative combinators
   * but they are uninteresting and we eliminate them in conjunctions and disjunctions.
   * @group Constructors
   */
  case object Pure extends Trace {
    def title = "<pure>"
  }

  /**
   * A trace indicating that a flatMap could not be performed due to prior failure.
   * @group Constructors
   */
  case object Halt extends Trace {
    def title = "<halt>"
  }

  /**
   * A trace indicating an error was raised.
   * @group Constructors
   */
  case class Error(message: String) extends Trace {
    def title  = message
    override def errors = List(message)
  }

  /**
   * A trace indicating that validation failed on a prior configuration.
   * @group Constructors
   */
  case class Invalid(message: String, child: Trace) extends CompositeTrace(NonEmptyChain(child)) {
    def title = message
    override def errors = message :: super.errors
  }

  /**
   * A trace indicating that a prior configuration is optional (and can never be incomplete).
   * @group Constructors
   */
  case class Optional(child: Trace, provided: Boolean) extends CompositeTrace(NonEmptyChain(child)) {
    def title = "optional"
    override def incomplete = false
    override def errors = title :: child.errors
  }

  /**
   * A trace indicating that an environment variable was read.
   * @group Constructors
   */
  case class Read(key: Key, value: Option[String]) extends Trace {
    def precis(s: String): String = {
      val filtered = s.exists(_.isControl)
      val sʹ  = s.map { c => if (c.isControl) '☐' else c } .take(40)
      s"$sʹ${if (sʹ.length() < s.length()) "⋯ <truncated>" else ""}${if (filtered) " <filtered>" else ""}"
    }
    def title = s"${key.name}${value.map(precis).map(s => if (key.secret) "<redacted>" else s).foldMap(v => s" = $v")}"
    override def incomplete = value.isEmpty
  }

  /**
   * A trace indicating that a configuration requires many prior configurations.
   * @group Constructors
   */
  case class Conjunction(elements: NonEmptyChain[Trace]) extends CompositeTrace(elements) {
    def title    = "all of the following"
  }
  /**
   * Module of factory methods for `Conjunction`.
   * @group Modules
   */
  object Conjunction {
    def apply(elem: Trace, elems: Trace*): Conjunction = apply(NonEmptyChain(elem, elems: _*))
  }

  /**
   * A trace indicating that a configuration requires one of many prior configurations.
   * @group Constructors
   */
  case class Disjunction(elements: NonEmptyChain[Trace]) extends CompositeTrace(elements) {
    def title    = "one of the following"
    override def incomplete = elements.forall(_.incomplete)
  }
  /**
   * Module of factory methods for `Disjunction`.
   * @group Modules
   */
  object Disjunction {
    def apply(elem: Trace, elems: Trace*): Disjunction = apply(NonEmptyChain(elem, elems: _*))
  }

}
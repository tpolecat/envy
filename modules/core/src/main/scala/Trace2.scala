// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy2

import cats._
import cats.data._
import cats.implicits._
import envy.Key

sealed abstract class Trace(
  val title:      String,
  val incomplete: Boolean      = false,
  val errors:     List[String] = Nil,
) {

  // These are overridden in Pure, Conjunction, and Disjunction
  def &&(that: Trace): Trace = if (that == Trace.Pure) this else Trace.Conjunction(NonEmptyChain(this, that))
  def ||(that: Trace): Trace = if (that == Trace.Pure) this else Trace.Disjunction(NonEmptyChain(this, that))

}

object Trace {

  implicit val EqTrace: Eq[Trace] = Eq.fromUniversalEquals

  /**
   * A trace that aggregates one or more prior traces.
   */
  sealed abstract class CompositeTrace(
        title:    String,
    val elements: NonEmptyChain[Trace]
  ) extends Trace(
    title      = title,
    incomplete = elements.exists(_.incomplete),
    errors     = elements.foldMap(_.errors)
  ) {
    def this(title:  String, element: Trace) =
      this(title, NonEmptyChain(element))
  }

  /**
   * A trace indicating a pure value was yielded. These proliferate via appliciative combinators
   * but they are uninteresting and we eliminate them in conjunctions and disjunctions.
   */
  case object Pure extends Trace(title = "<pure>") {
    override def &&(that: Trace): Trace = that
  }

  /**
   * A trace indicating that a flatMap could not be performed because a prior configuration failed.
   */
  case object Halt extends Trace(title = "<halt>")

  /**
   * A trace indicating an error was raised.
   * @param message
   */
  case class Error(
    message: String
  ) extends Trace(
    title  = message,
    errors = List(message)
  )

  /**
   * A trace indicating that validation was performed on a prior configuration.
   * @param message
   * @param child
   */
  case class Validation(
    message: String,
    child:   Trace,
    valid:   Boolean,
  ) extends CompositeTrace(
    title    = message,
    elements = NonEmptyChain(child),
  ) {
    override val errors = if (valid) child.errors else (title :: child.errors)
  }

  /**
   * A trace indicating that a prior configuration is optional (and can never be incomplete).
   * @param child
   * @param provided
   */
  case class Optional(
    child:    Trace,
    provided: Boolean,
  ) extends CompositeTrace(
    title    = "optional",
    elements = NonEmptyChain(child)
  ) {
    override val incomplete = false
    override val errors = title :: child.errors
  }

  /**
   * A trace indicating that an environment variable was read.
   * @param key
   * @param value
   */
  case class Read(
    key:   Key,
    value: Option[String]
  ) extends Trace(
    title      = s"${key.name}${key.description.foldMap(v => s": $v")}",
    incomplete = value.isEmpty,
  )

  /**
   * A trace indicating that a configuration requires many prior configurations.
   * @param elements
   */
  case class Conjunction(
    override val elements: NonEmptyChain[Trace]
  ) extends CompositeTrace(
    title    = "all of the following",
    elements = elements
  ) {
    override def &&(that: Trace): Trace =
      that match {
        case Conjunction(es) => Conjunction(elements :++ es.filterNot(elements.contains))
        case that     => this && Conjunction(NonEmptyChain(that))
      }
  }

  /**
   * A trace indicating that a configuration requires one of many prior configurations.
   * @param elements
   */
  case class Disjunction(
    override val elements: NonEmptyChain[Trace]
  ) extends CompositeTrace(
    title    = "one of the following",
    elements = elements
  ) {
    override val incomplete = elements.forall(_.incomplete)
    override def &&(that: Trace): Trace =
      that match {
        case Disjunction(es) => Disjunction(elements :++ es.filterNot(elements.contains))
        case that     => this || Disjunction(NonEmptyChain(that))
      }
  }

}
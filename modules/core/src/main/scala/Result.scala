package envy

sealed trait Result[+A] {
  def trace: Trace
}

object Result {
  case class Success[A](trace: Trace, value: A) extends Result[A]
  case class Incomplete(trace: Trace) extends Result[Nothing]
  case class Error(trace: Trace, message: String) extends Result[Nothing]
}

// seems like we want (Trace, List[Key], List[String], Option[A])


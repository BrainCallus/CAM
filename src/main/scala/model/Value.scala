package model

import cats.Show
import cats.syntax.show.*

sealed trait Value

object Value {
  case object NullValue extends Value {
    override def toString: String = "()"
  }

  case class BoolValue(value: Boolean) extends Value

  case class IntValue(value: Int) extends Value

  case class StringValue(value: String) extends Value

  // todo _3 for debug only
  case class PairValue(first: (Int, List[Value] => Value), second: (Int, List[Value] => Value)) extends Value {
    def showValues(env: List[Value]): String =
      if (env.isDefinedAt(first._1) && env.isDefinedAt(second._1)) {
        s"(${env(first._1).show}, ${env(second._1).show})"
      } else {
        s"Could not retrieve pair"
      }
  }

  case class ClosureValue(code: List[Instruction]) extends Value

  implicit val valueOrdering: Ordering[Value] = (x: Value, y: Value) =>
    (x, y) match {
      case (NullValue, NullValue)           => 0
      case (IntValue(a), IntValue(b))       => a.compareTo(b)
      case (BoolValue(a), BoolValue(b))     => a.compareTo(b)
      case (StringValue(a), StringValue(b)) => a.compareTo(b)
      case (PairValue(f1, s1), PairValue(f2, s2)) =>
        val firstComp = f1._1.compare(f2._1)
        if (firstComp != 0) firstComp else s1._1.compare(s2._1)
      case (NullValue, _)                     => -1
      case (_, NullValue)                     => 1
      case (IntValue(_), _)                   => -1
      case (_, IntValue(_))                   => 1
      case (BoolValue(_), _)                  => -1
      case (_, BoolValue(_))                  => 1
      case (StringValue(_), _)                => -1
      case (_, StringValue(_))                => 1
      case (_: PairValue, _)                  => -1
      case (_, _: PairValue)                  => 1
      case (ClosureValue(_), ClosureValue(_)) => 0
    }

  implicit val valueShow: Show[Value] = Show.show {
    case NullValue          => "()"
    case BoolValue(value)   => value.toString
    case IntValue(value)    => value.toString
    case StringValue(value) => value
    case PairValue(first, second) =>
      s"(&${first._1}, &${second._1})"
    case ClosureValue(code) => s"Closure(${code.map(_.show).mkString("[", ",", "]")})"
  }
}

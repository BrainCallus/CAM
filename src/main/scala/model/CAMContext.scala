package model

import cats.Show
import cats.syntax.show.*
import model.Value.*
import model.io.ContextWriter.{showCode, showIterableStrings}

case class CAMContext(
  term: Value,
  code: List[Instruction],
  stack: List[Value],
  env: List[Value],
)

object CAMContext {
  implicit val camContextShow: Show[CAMContext] = Show.show { case CAMContext(term, code, stack, env) =>
    s"(${showValue(term, env)} | ${showCode.show(code)} | ${showIterableStrings.show(stack.map(showValue(_, env)))})"
  }

  private def showValue(value: Value, env: List[Value]): String = {
    value match {
      case p: PairValue => p.showValues(env)
      case v            => v.show
    }
  }
}

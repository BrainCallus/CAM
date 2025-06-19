package model

import cats.Show
import cats.syntax.show.*
import model.Value.*

case class CAMContext(
  term: Value,
  code: List[Instruction],
  stack: List[Value],
  env: List[Value],
) {}

object CAMContext {
  private type TripleString = (String, String, String)
  implicit val camContextShow: Show[CAMContext] = Show.show { case CAMContext(term, code, stack, env) =>
    val termShow  = showValue(term, env)
    val stackShow = stack.map(showValue(_, env)).mkString("[", ",", "]")
    RowFormater.formatRow(
      List(
        (termShow, 50),
        (code.show, 50),
        (stackShow, 100),
      ),
      sep = Some(" | "),
    )
  }

  private def showValue(value: Value, env: List[Value]): String = {
    value match {
      case p: PairValue => p.showValues(env)
      case v            => v.show
    }
  }
}

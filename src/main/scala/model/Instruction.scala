package model

import cats.Show
import cats.syntax.either.*
import cats.syntax.show.*
import enumeratum.{Enum, EnumEntry}
import model.CAMContext
import model.Value.*

sealed trait Instruction extends EnumEntry {
  def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value]

  def executeSingle: CAMContext => Either[Throwable, CAMContext]
}

object Instruction extends Enum[Instruction] {
  case object STOP extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (_, v :: _) => v.asRight
      case _           => new RuntimeException("STOP instruction requires value on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, Nil, stack, env) => CAMContext(term, Nil, stack, env).asRight
      case _ =>
        new RuntimeException("Unexpected STOP instruction: further instructions exist but can't be executed").asLeft
    }
  }

  /** Fst
    */
  case object CAR extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, st @ PairValue(f, _) :: xs) => i.execute(env)(is, f._2(env) :: xs)
      case _ => new RuntimeException("CAR instruction requires a pair on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(p @ PairValue(f, _), code, stack, env) =>
        CAMContext(f._2(env), code, stack, env).asRight
      case _ => new RuntimeException("CAR instruction requires pair value a term").asLeft
    }
  }

  /** Snd
    */
  case object CDR extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, st @ PairValue(_, s) :: xs) => i.execute(env)(is, s._2(env) :: xs)
      case _ => new RuntimeException("CAR instruction requires a pair on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(p @ PairValue(_, s), code, stack, env) =>
        CAMContext(s._2(env), code, stack, env).asRight
      case _ => new RuntimeException("CDR instruction requires pair value a term").asLeft
    }
  }

  case object SWAP extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v1 :: v2 :: xs) => i.execute(env)(is, v2 :: v1 :: xs)
      case _ => new RuntimeException("SWAP instruction requires at least two values on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, v2 :: stack, env) =>
        CAMContext(v2, code, term :: stack, env).asRight
      case _ => new RuntimeException("SWAP instruction requires at least one value on the stack").asLeft
    }
  }

  case object PUSH extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v :: xs) => i.execute(env)(is, v :: v :: xs)
      case _ => new RuntimeException("PUSH instruction requires at least one value on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, stack, env) =>
        CAMContext(term, code, term :: stack, env).asRight
    }
  }

  case object CONS extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v1 :: v2 :: xs) =>
        val (p, newTerm) = mkPair(env, v1, v2)
        i.execute(newTerm)(is, p :: xs)

      case _ => new RuntimeException("CONS instruction requires at least two values on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, v2 :: stack, env) =>
        val (p, newEnv) = mkPair(env, term, v2)
        CAMContext(p, code, stack, newEnv).asRight
      case _ => new RuntimeException("CONS instruction requires at least one value on the stack").asLeft
    }
  }

  final case class BRANCH(thenCase: Value, elseCase: Value) extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, BoolValue(true) :: stack)  => i.execute(env)(is, thenCase :: stack)
      case (i :: is, BoolValue(false) :: stack) => i.execute(env)(is, elseCase :: stack)
      case _ => new RuntimeException("BRANCH instruction requires a boolean value on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(BoolValue(cond), code, stack, env) =>
        CAMContext(if (cond) thenCase else elseCase, code, stack, env).asRight
      case _ => new RuntimeException("BRANCH instruction requires a boolean value as a term").asLeft
    }
  }

  final case class QUOTE(value: Value) extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, _v :: stack) => i.execute(env)(is, value :: stack)
      case _                      => new RuntimeException("QUOTE instruction requires a value to be pushed").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = { case CAMContext(_, code, stack, env) =>
      CAMContext(value, code, stack, env).asRight
    }
  }

  case object SPLIT extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, PairValue(f, s) :: xs) => i.execute(env)(is, f._2(env) :: s._2(env) :: xs)
      case _ => new RuntimeException("SPLIT instruction requires a pair on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(p @ PairValue(f, s), code, stack, env) =>
        CAMContext(f._2(env), code, s._2(env) :: stack, env).asRight
      case _ => new RuntimeException("SPLIT instruction requires a pair as a term").asLeft
    }
  } // (p1, p2) :: tail -> p1 :: p2 :: tail

  case object SHL3 extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, h1 :: h2 :: h3 :: tail) => i.execute(env)(is, h2 :: h3 :: h1 :: tail)
      case (i, s) => new RuntimeException("SHL3 instruction requires at least three values on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, h2 :: h3 :: stack, env) =>
        CAMContext(h2, code, h3 :: term :: stack, env).asRight
      case _ => new RuntimeException("SHL3 instruction requires at least two values on the stack").asLeft
    }
  } // h1 :: h2 :: h3 :: tail -> h2 :: h3 :: h1 :: tail

  case object SHR3 extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, h1 :: h2 :: h3 :: tail) => i.execute(env)(is, h3 :: h1 :: h2 :: tail)
      case _ => new RuntimeException("SHR3 instruction requires at least three values on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, h2 :: h3 :: stack, env) =>
        CAMContext(h3, code, term :: h2 :: stack, env).asRight
      case _ => new RuntimeException("SHR3 instruction requires at least two values on the stack").asLeft
    }
  } // h1 :: h2 :: h3 :: tail -> h3 :: h1 :: h2 :: tail

  case object SETFST extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v :: PairValue(f, s) :: xs) =>
        val newTerm = env.updated(f._1, v) // todo: make safe
        i.execute(newTerm)(is, PairValue(f, s) :: xs)
      case _ => new RuntimeException("SETFST instruction requires a pair on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, PairValue(f, s) :: stack, env) =>
        val newValues = env.updated(f._1, term) // todo: make safe
        CAMContext(PairValue(f, s), code, stack, newValues).asRight
      case _ => new RuntimeException("SETFST instruction requires a pair as a term").asLeft
    }
  }

  case object SETSND extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v :: PairValue(f, s) :: xs) =>
        val newTerm = env.updated(s._1, v) // todo: make safe
        i.execute(newTerm)(is, PairValue(f, s) :: xs)
      case _ => new RuntimeException("SETSND instruction requires a pair on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, PairValue(f, s) :: stack, env) =>
        val newValues = env.updated(s._1, term) // todo: make safe
        CAMContext(PairValue(f, s), code, stack, newValues).asRight
      case _ => new RuntimeException("SETSND instruction requires a pair as a term").asLeft
    }
  }

  case object RET extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (_, v :: ClosureValue(i :: is) :: stack) =>
        i.execute(env)(is, v :: stack)
      case _ => new RuntimeException("RET instruction requires a value to return").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, _, ClosureValue(otherCode) :: stack, env) =>
        CAMContext(term, otherCode, stack, env).asRight
      case _ => new RuntimeException("RET instruction requires a value to return").asLeft
    }
  }

  case object CALL extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (instrs, ClosureValue(i :: is) :: v :: stack) =>
        i.execute(env)(is, v :: (ClosureValue(instrs) :: stack))
      case _ => new RuntimeException("CALL instruction requires a closure and an argument on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(c @ ClosureValue(otherCode), code, v :: stack, env) =>
        CAMContext(v, otherCode, ClosureValue(code) :: stack, env).asRight
      case _ =>
        new RuntimeException("CALL instruction requires a closure value as term and argument on the stack").asLeft
    }
  }

  case object ADD extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) => i.execute(env)(is, IntValue(x + y) :: xs)
      case _ => new RuntimeException("ADD instruction requires two integers on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(IntValue(x), code, IntValue(y) :: stack, env) =>
        CAMContext(IntValue(x + y), code, stack, env).asRight
      case _ => new RuntimeException("EQ instruction requires a value on the stack").asLeft
    }
  }

  case object SUB extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) =>
        i.execute(env)(is, IntValue(x - y) :: xs)
      case _ => new RuntimeException("SUB instruction requires two integers on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(IntValue(x), code, IntValue(y) :: stack, env) =>
        CAMContext(IntValue(x - y), code, stack, env).asRight
      case _ => new RuntimeException("EQ instruction requires a value on the stack").asLeft
    }
  }

  case object MUL extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) =>
        i.execute(env)(is, IntValue(x * y) :: xs)
      case _ => new RuntimeException("MUL instruction requires two integers on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(IntValue(x), code, IntValue(y) :: stack, env) =>
        CAMContext(IntValue(x * y), code, stack, env).asRight
      case _ => new RuntimeException("EQ instruction requires a value on the stack").asLeft
    }
  }

  case object DIV extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, _ :: IntValue(0) :: _) =>
        new RuntimeException("Division by zero is not allowed").asLeft
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) =>
        i.execute(env)(is, IntValue(x / y) :: xs)
      case _ => new RuntimeException("DIV instruction requires two integers on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(IntValue(value), code, IntValue(0) :: stack, env) =>
        new RuntimeException("Division by zero is not allowed").asLeft
      case CAMContext(IntValue(value), code, IntValue(d) :: stack, env) =>
        CAMContext(IntValue(value / d), code, stack, env).asRight
      case _ => new RuntimeException("GT instruction requires a value on the stack").asLeft
    }
  }

  case object EQ extends Instruction {
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v1 :: v2 :: xs) =>
        i.execute(env)(is, BoolValue(v1 == v2) :: xs)
      case _ => new RuntimeException("EQ instruction requires two comparable values on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, v :: stack, env) => CAMContext(BoolValue(term == v), code, stack, env).asRight
      case _ => new RuntimeException("EQ instruction requires a value on the stack").asLeft
    }
  }

  case object LT extends Instruction {
    import scala.math.Ordering.Implicits.infixOrderingOps
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, x :: y :: xs) => i.execute(env)(is, BoolValue(x < y) :: xs)
      case _                       => new RuntimeException("LT instruction requires two integers on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, v :: stack, env) => CAMContext(BoolValue(term < v), code, stack, env).asRight
      case _ => new RuntimeException("LT instruction requires a value on the stack").asLeft
    }
  }

  case object GT extends Instruction {
    import scala.math.Ordering.Implicits.infixOrderingOps
    override def execute(env: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, x :: y :: xs) => i.execute(env)(is, BoolValue(x > y) :: xs)
      case _                       => new RuntimeException("GT instruction requires two integers on the stack").asLeft
    }

    override def executeSingle: CAMContext => Either[Throwable, CAMContext] = {
      case CAMContext(term, code, v :: stack, env) => CAMContext(BoolValue(term > v), code, stack, env).asRight
      case _ => new RuntimeException("GT instruction requires a value on the stack").asLeft
    }
  }

  override def values: IndexedSeq[Instruction] = findValues

  implicit val instructionShow: Show[Instruction] = Show.show {
    case BRANCH(thenCase, elseCase) => s"BRANCH(${thenCase.show}, ${elseCase.show})"
    case QUOTE(value)               => s"QUOTE(${value.show})"
    case i                          => i.toString
  }

  private def mkPair(values: List[Value], v1: Value, v2: Value): (PairValue, List[Value]) = {
    val i      = values.length
    val newEnv = values ++ List(v1, v2)
    (PairValue((i, arr => arr(i)), (i + 1, arr => arr(i + 1))), newEnv)
  }
}

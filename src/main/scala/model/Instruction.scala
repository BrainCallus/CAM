package model

import enumeratum.{Enum, EnumEntry}
import cats.syntax.either.*
import model.Value.{BoolValue, ClosureValue, IntValue, PairValue}

sealed trait Instruction extends EnumEntry {
  def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value]
}

object Instruction extends Enum[Instruction] {
  case object STOP extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (_, v :: _) => v.asRight
      case _           => new RuntimeException("STOP instruction requires value on the stack").asLeft
    }
  }

  /** Fst
    */
  case object CAR extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, st @ PairValue(f, _) :: xs) => i.execute(term)(is, f._2(term) :: xs)
      case _ => new RuntimeException("CAR instruction requires a pair on the stack").asLeft
    }
  }

  /** Snd
    */
  case object CDR extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, st @ PairValue(_, s) :: xs) => i.execute(term)(is, s._2(term) :: xs)
      case _ => new RuntimeException("CAR instruction requires a pair on the stack").asLeft
    }
  }

  final case class PUSH(value: Value) extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, stack) => i.execute(term)(is, value :: stack)
      case _ => new RuntimeException("PUSH instruction requires a value to be pushed onto the stack").asLeft
    }
  }

  case object SWAP extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v1 :: v2 :: xs) => i.execute(term)(is, v2 :: v1 :: xs)
      case _ => new RuntimeException("SWAP instruction requires at least two values on the stack").asLeft
    }
  }

  case object DUPLICATE extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v :: xs) => i.execute(term)(is, v :: v :: xs)
      case _ => new RuntimeException("DUPLICATE instruction requires at least one value on the stack").asLeft
    }
  }

  case object CONS extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v1 :: v2 :: xs) =>
        val (p, newTerm) = mkPair(term, v1, v2)
        i.execute(newTerm)(is, p :: xs)

      case _ => new RuntimeException("CONS instruction requires at least two values on the stack").asLeft
    }
  }

  private def mkPair(values: List[Value], v1: Value, v2: Value): (PairValue, List[Value]) = {
    val i       = values.length
    val newTerm = values ++ List(v1, v2)
    (PairValue((i, arr => arr(i), v1), (i + 1, arr => arr(i + 1), v2)), newTerm)
  }

  final case class BRANCH(thenCase: Value, elseCase: Value) extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, BoolValue(true) :: stack)  => i.execute(term)(is, thenCase :: stack)
      case (i :: is, BoolValue(false) :: stack) => i.execute(term)(is, elseCase :: stack)
      case _ => new RuntimeException("BRANCH instruction requires a boolean value on the stack").asLeft
    }
  }

  final case class QUOTE(value: Value) extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, _v :: stack) => i.execute(term)(is, value :: stack)
      case _                      => new RuntimeException("QUOTE instruction requires a value to be pushed").asLeft
    }
  }

  case object SPLIT extends Instruction {
    override def execute(values: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, PairValue(f, s) :: xs) => i.execute(values)(is, f._2(values) :: s._2(values) :: xs)
      case _ => new RuntimeException("SPLIT instruction requires a pair on the stack").asLeft
    }
  } // (p1, p2) :: tail -> p1 :: p2 :: tail

  case object SHL3 extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, h1 :: h2 :: h3 :: tail) => i.execute(term)(is, h2 :: h3 :: h1 :: tail)
      case (i, s) => new RuntimeException("SHL3 instruction requires at least three values on the stack").asLeft
    }
  } // h1 :: h2 :: h3 :: tail -> h2 :: h3 :: h1 :: tail

  case object SHR3 extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, h1 :: h2 :: h3 :: tail) => i.execute(term)(is, h3 :: h1 :: h2 :: tail)
      case _ => new RuntimeException("SHR3 instruction requires at least three values on the stack").asLeft
    }
  } // h1 :: h2 :: h3 :: tail -> h3 :: h1 :: h2 :: tail

  case object SETFST extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v :: PairValue(f, s) :: xs) =>
        val newTerm = term.updated(f._1, v) // todo: make safe
        i.execute(newTerm)(is, PairValue(f.copy(_3 = v), s) :: xs)
      case _ => new RuntimeException("SETFST instruction requires a pair on the stack").asLeft
    }
  }

  case object SETSND extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v :: PairValue(f, s) :: xs) =>
        val newTerm = term.updated(s._1, v) // todo: make safe
        i.execute(newTerm)(is, PairValue(f, s.copy(_3 = v)) :: xs)
      case _ => new RuntimeException("SETSND instruction requires a pair on the stack").asLeft
    }
  }

  case object RET extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (_, v :: ClosureValue(i :: is) :: stack) =>
        i.execute(term)(is, v :: stack)
      case _ => new RuntimeException("RET instruction requires a value to return").asLeft
    }
  }

  case object CALL extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (instrs, ClosureValue(i :: is) :: v :: stack) =>
        i.execute(term)(is, v :: (ClosureValue(instrs) :: stack))
      case _ => new RuntimeException("CALL instruction requires a closure and an argument on the stack").asLeft
    }
  }

  case object ADD extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) => i.execute(term)(is, IntValue(x + y) :: xs)
      case _ => new RuntimeException("ADD instruction requires two integers on the stack").asLeft
    }
  }

  case object SUB extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) =>
        i.execute(term)(is, IntValue(x - y) :: xs)
      case _ => new RuntimeException("SUB instruction requires two integers on the stack").asLeft
    }
  }

  case object MUL extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) =>
        i.execute(term)(is, IntValue(x * y) :: xs)
      case _ => new RuntimeException("MUL instruction requires two integers on the stack").asLeft
    }
  }

  case object DIV extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, _ :: IntValue(0) :: _) =>
        new RuntimeException("Division by zero is not allowed").asLeft
      case (i :: is, IntValue(x) :: IntValue(y) :: xs) =>
        i.execute(term)(is, IntValue(x / y) :: xs)
      case _ => new RuntimeException("DIV instruction requires two integers on the stack").asLeft
    }
  }

  case object EQ extends Instruction {
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, v1 :: v2 :: xs) =>
        i.execute(term)(is, BoolValue(v1 == v2) :: xs)
      case _ => new RuntimeException("EQ instruction requires two comparable values on the stack").asLeft
    }
  }

  case object LT extends Instruction {
    import scala.math.Ordering.Implicits.infixOrderingOps
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, x :: y :: xs) => i.execute(term)(is, BoolValue(x < y) :: xs)
      case _                       => new RuntimeException("LT instruction requires two integers on the stack").asLeft
    }
  }

  case object GT extends Instruction {
    import scala.math.Ordering.Implicits.infixOrderingOps
    override def execute(term: List[Value]): (List[Instruction], List[Value]) => Either[Throwable, Value] = {
      case (i :: is, x :: y :: xs) => i.execute(term)(is, BoolValue(x > y) :: xs)
      case _                       => new RuntimeException("GT instruction requires two integers on the stack").asLeft
    }
  }

  override def values: IndexedSeq[Instruction] = findValues
}

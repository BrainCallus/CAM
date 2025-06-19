package model.expression

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import model.*
import model.Instruction._
import model.Value._

sealed trait Expression extends ToInstructions

object Expression {
  final case class IntTerm(value: Int) extends Expression {
    override def toInstructions[F[_]](envs: List[String])(implicit F: MonadCancelThrow[F]): F[List[Instruction]] =
      List(Instruction.QUOTE(IntValue(value))).pure[F]
  }

  final case class StringTerm(value: String) extends Expression {
    override def toInstructions[F[_]](envs: List[String])(implicit F: MonadCancelThrow[F]): F[List[Instruction]] =
      List(Instruction.QUOTE(StringValue(value))).pure[F]
  }

  final case class BooleanTerm(value: Boolean) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.QUOTE(BoolValue(value))).pure[F]
  }

  final case class PairTerm(fst: Expression, snd: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        fstInst <- fst.toInstructions[F](envs)
        sndInst <- snd.toInstructions[F](envs)
      } yield Instruction.PUSH :: sndInst ++ (Instruction.SWAP :: fstInst) ++ List(Instruction.CONS)
  }

  final case class VarTerm(name: String) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] = {
      envs match {
        case Nil =>
          MonadCancelThrow[F].raiseError(
            new RuntimeException(s"Couldn't transform to Instruction: variable $name is unbound"),
          )
        case h :: t =>
          if (h == name) {
            List(Instruction.CAR).pure[F] // If the variable is the first in the environment, we pop it
          } else {
            toInstructions[F](t).map(Instruction.CDR :: _)
          }
      }
    }
  }

  final case class UnaryOpTerm(op: UnaryOperation, term: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        termInst <- term.toInstructions[F](envs)
        opInst   <- op.toInstructions[F](envs)
      } yield termInst
  }

  final case class BinaryOpTerm(op: BinaryOperation, left: Expression, right: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        leftInst  <- left.toInstructions[F](envs)
        rightInst <- right.toInstructions[F](envs)
        opInst    <- op.toInstructions[F](envs)
      } yield (Instruction.PUSH :: rightInst) ++
        (Instruction.SWAP :: leftInst) ++
        opInst
  }

  final case class CondTerm(cond: Expression, thenTerm: Expression, elseTerm: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        condInst <- cond.toInstructions[F](envs)
        thenInst <- thenTerm.toInstructions[F](envs)
        elseInst <- elseTerm.toInstructions[F](envs)
      } yield (Instruction.PUSH :: condInst) ++
        List(
          Instruction
            .BRANCH(ClosureValue(thenInst ++ List(Instruction.RET)), ClosureValue(elseInst ++ List(Instruction.RET))),
          Instruction.CALL,
        )
  }

  final case class FuncTerm(name: String, body: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        bodyInst <- body.toInstructions[F](name :: envs)
      } yield List(
        Instruction.PUSH,
        Instruction.QUOTE(ClosureValue(bodyInst.appended(Instruction.RET))),
        Instruction.SWAP,
        Instruction.CONS,
      )
  }

  final case class AppTerm(func: Expression, arg: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        funcInst <- func.toInstructions[F](envs)
        argInst  <- arg.toInstructions[F](envs)
      } yield (Instruction.PUSH :: argInst) ++
        (Instruction.SWAP :: funcInst) ++
        List(
          Instruction.SPLIT,
          Instruction.SHR3,
          Instruction.CONS,
          Instruction.SWAP,
          Instruction.CALL,
        )
  }

  final case class LetTerm(name: String, value: Expression, in: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      for {
        valueInst <- value.toInstructions[F](envs)
        inInst    <- in.toInstructions[F](name :: envs)
      } yield (Instruction.PUSH :: valueInst) ++ (Instruction.CONS :: inInst)
  }

  final case class LetRecTerm(name: String, value: Expression, in: Expression) extends Expression {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      value match {
        case ft: FuncTerm =>
          for {
            valueInst <- ft.toInstructions[F](name :: envs)
            inInst    <- in.toInstructions[F](name :: envs)
          } yield (Instruction.PUSH :: Instruction.QUOTE(NullValue) :: valueInst) ++
            List(
              Instruction.PUSH,
              Instruction.SHL3,
              Instruction.CONS,
              Instruction.SETFST,
              Instruction.CAR,
            ) ++ inInst
        case term =>
          for {
            valueInst <- term.toInstructions[F](envs)
            inInst    <- in.toInstructions[F](name :: envs)
          } yield List(
            Instruction.PUSH,
            Instruction.PUSH,
            Instruction.QUOTE(NullValue),
            Instruction.PUSH,
            Instruction.CONS,
            Instruction.PUSH,
            Instruction.SHL3,
            Instruction.CONS,
          ) ++ valueInst ++
            List(
              Instruction.PUSH,
              Instruction.SHL3,
              Instruction.CAR,
              Instruction.SETFST,
              Instruction.SWAP,
              Instruction.CDR,
              Instruction.SETSND,
              Instruction.CONS,
            ) ++ inInst
      }

  }
}

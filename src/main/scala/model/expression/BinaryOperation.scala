package model.expression

import cats.effect.MonadCancelThrow
import cats.syntax.applicative._
import enumeratum.{Enum, EnumEntry}
import model.{Instruction, ToInstructions}

sealed trait BinaryOperation extends EnumEntry with ToInstructions

object BinaryOperation extends Enum[BinaryOperation] {
  case object Add extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.ADD).pure[F]
  }

  case object Sub extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.SUB).pure[F]
  }

  case object Mul extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.MUL).pure[F]
  }

  case object Div extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.DIV).pure[F]
  }

  case object Eq extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.EQ).pure[F]
  }

  case object Lt extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.LT).pure[F]
  }

  case object Gt extends BinaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.GT).pure[F]
  }

  override def values: IndexedSeq[BinaryOperation] = findValues
}

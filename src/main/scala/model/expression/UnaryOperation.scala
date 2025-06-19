package model.expression

import cats.effect.kernel.MonadCancelThrow
import cats.syntax.applicative.*
import enumeratum.{Enum, EnumEntry}
import model.{Instruction, ToInstructions}

sealed trait UnaryOperation extends EnumEntry with ToInstructions

object UnaryOperation extends Enum[UnaryOperation] {
  case object Fst extends UnaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.CAR).pure[F]
  }
  case object Snd extends UnaryOperation {
    override def toInstructions[F[_]: MonadCancelThrow](envs: List[String]): F[List[Instruction]] =
      List(Instruction.CDR).pure[F]
  }

  override def values: IndexedSeq[UnaryOperation] = findValues
}

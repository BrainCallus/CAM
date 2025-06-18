package model

import cats.effect.kernel.MonadCancelThrow

trait ToInstructions {
  // todo simplify to Either[Err, List[Instruction]]
  def toInstructions[F[_] : MonadCancelThrow](envs: List[String]): F[List[Instruction]]
}

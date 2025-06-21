package model

import cats.effect.MonadCancelThrow
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import model.Instruction.STOP
import model.Value.NullValue
import model.expression.Expression
import model.io.ContextWriter
import tofu.syntax.feither.*

trait CAMExecutor[F[_]] {
  def executeInstructions(instructions: List[Instruction]): F[Value]
  def executeExpression(expr: Expression): F[Value]
}

object CAMExecutor {
  def apply[F[_]](implicit executor: CAMExecutor[F]): CAMExecutor[F] = executor

  def make[F[_]: MonadCancelThrow: ContextWriter]: CAMExecutor[F] = new CAMExecutor[F] {
    override def executeInstructions(instructions: List[Instruction]): F[Value] = {
      val startCtx = CAMContext(term = NullValue, code = instructions, stack = List.empty, env = List.empty)

      MonadCancelThrow[F]
        .tailRecM[CAMContext, Either[Throwable, Value]](startCtx) { ctx =>
          for {
            _ <- ContextWriter[F].consolePrint(ctx)
            r <- ctx.code match {
              case STOP :: Nil => ctx.term.asRight.asRightF
              case i :: is =>
                i.executeSingle(ctx.copy(code = is)) match {
                  case Right(newCtx) => newCtx.asLeftF
                  case Left(err)     => err.asLeft.asRightF
                }
              case Nil => new RuntimeException("Unexpected instructions end").asLeft.asRightF
            }
          } yield r
        }
        .foldF(_.raiseError[F, Value], _.pure[F])
    }

    override def executeExpression(expr: Expression): F[Value] =
      expr.toInstructions[F](List.empty).flatMap(i => executeInstructions(i.appended(STOP)))
  }
}

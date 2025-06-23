package model.parser.util

import cats.Monad
import cats.data.{EitherT, StateT}
import model.parser.template.AbstractLexer

import java.text.ParseException

object TypeOps {
  type ParseEither[A]         = Either[ParseException, A]
  type MET[E[_], A]           = EitherT[E, ParseException, A]
  type ParseState[F[_], S, A] = StateT[MkContainer[MET, F]#Cont, S, A]

  def throwPEStateT[F[_]: Monad, L <: AbstractLexer[?], T](
    message: String,
  ): ParseState[F, L, T] =
    StateT.apply[MkContainer[MET, F]#Cont, L, T] { l => EitherT.leftT(new ParseException(message, l.curPos())) }

  trait MkContainer[C[_[_], _], F[_]] {
    type Cont[A] = C[F, A]
  }
}

package model.parser.template

import model.parser.grammar.{LexerRule, Token}

import java.io.InputStream
import AbstractLexer.LexerParams
import Tokenized.{TokenizedEmpty, TokenizedValue}
import model.parser.util.Ternary.Ternary
import model.parser.util.TypeOps.ParseEither

import java.text.ParseException
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.immutable.Queue

abstract class AbstractLexer[T <: Token](
  inputStream: InputStream,
  lexerParams: LexerParams,
) {
  def apply[L <: AbstractLexer[_ <: Token]](inputStream: InputStream): L = this(inputStream)

  val lexerRules: List[LexerRule[T]]
  def getTokenWithName(name: String): T

  def curPos(): Int = lexerParams.curPos

  def curToken(): Tokenized = lexerParams.tokenized

  def curTokenIn(set: Set[T]): Boolean =
    curToken() match {
      case TokenizedEmpty                 => false
      case TokenizedValue(token, _, _, _) => set.contains(token.asInstanceOf[T])
    }

  def compareToken(expected: T): Boolean = {
    curToken() match {
      case TokenizedEmpty => false
      case t              => t.asInstanceOf[TokenizedValue[T]].token == expected
    }
  }

  def nextToken(): ParseEither[LexerParams] = processCurToken(this.lexerParams, skip = true)

  @tailrec
  private def processCurToken(
    curParams: LexerParams,
    skip: Boolean,
    curTokenized: Tokenized = TokenizedEmpty,
  ): ParseEither[LexerParams] = {
    if (!skip) {
      Right(LexerParams(curTokenized, curParams.curPos, curParams.realPos, curParams.lastRead, curParams.buffer))
    } else {
      processCandidates(lexerRules.distinct, curParams, curParams.curPos, TokenizedEmpty) match {
        case Left(e) => Left(e)
        case Right((newParams, nPos, newTokenized)) =>
          val nextParams = removeProcessedChars(nPos)(newParams)
          processCurToken(nextParams, newTokenized.asInstanceOf[TokenizedValue[T]].skip, newTokenized)
      }
    }
  }

  @tailrec
  private def processCandidates(
    candidates: List[LexerRule[T]],
    params: LexerParams,
    nPos: Int,
    curTokenized: Tokenized,
  ): ParseEither[(LexerParams, Int, Tokenized)] = {
    if (!params.signalizeEnd && candidates.nonEmpty) {
      val nCandidates = List.empty[LexerRule[T]]
      val res = candidates.foldLeft((nCandidates, params, nPos, curTokenized))((tuple, candidate) => {
        processCandidate(candidate, params, tuple._3)(tuple._1, tuple._4)
      })
      val newParams = nextChar(params)
      processCandidates(res._1, newParams, res._3, res._4)
    } else {
      val newTokenized =
        params.signalizeEnd ?? (TokenizedValue(getTokenWithName("EOF"), "", params.curPos, skip = false), curTokenized)
      if (newTokenized == TokenizedEmpty) {
        Left(new ParseException("Recognition error at " + getBufferedString(params.buffer), params.curPos))
      } else {
        Right((params, nPos, newTokenized))
      }
    }
  }

  private def processCandidate(candidate: LexerRule[T], params: LexerParams, nPos: Int)(
    curCandidates: List[LexerRule[T]],
    curTokenized: Tokenized,
  ): (List[LexerRule[T]], LexerParams, Int, Tokenized) = {
    val curStr  = getBufferedString(params.buffer)
    val matcher = Pattern.compile(candidate.token.getPatternAsString).matcher(curStr)
    matcher.matches()
    val newCandidates = curCandidates ++ ((matcher.hitEnd() && params.lastRead != -1) ?? (Set(candidate), Set.empty))
    if (matcher.lookingAt()) {
      if (curTokenized == TokenizedEmpty || curTokenized.asInstanceOf[TokenizedValue[T]].tokenLen < matcher.end()) {
        val tokenized =
          TokenizedValue(candidate.token, curStr.substring(0, matcher.end()), params.curPos, candidate.skip)
        (newCandidates, params, params.curPos + matcher.end(), tokenized)
      } else {
        (newCandidates, params, nPos, curTokenized)
      }
    } else {
      (newCandidates, params, nPos, curTokenized)
    }
  }

  private def getBufferedString(queue: Queue[Char]) = {
    queue.foldLeft(new StringBuilder())((builder, ch) => builder.append(ch)).toString()
  }

  @tailrec
  private def removeProcessedChars(newPos: Int)(params: LexerParams): LexerParams = {
    if (params.curPos == newPos) {
      params
    } else {
      removeProcessedChars(newPos)(
        LexerParams(
          params.tokenized,
          params.curPos + 1,
          params.realPos,
          params.lastRead,
          (params.buffer.size > 1) ?? (params.buffer.tail, Queue.empty),
        ),
      )
    }
  }

  private def nextChar(params: LexerParams): LexerParams = {
    val read = inputStream.read()
    if (read != -1) {
      LexerParams(params.tokenized, params.curPos, params.realPos + 1, read, params.buffer.appended(read.toChar))
    } else {
      LexerParams(params.tokenized, params.curPos, params.realPos, read, params.buffer)
    }
  }
}

object AbstractLexer {
  case class LexerParams(
    tokenized: Tokenized,
    curPos: Int,
    realPos: Int,
    lastRead: Int,
    buffer: Queue[Char] = Queue.empty,
  ) {
    def signalizeEnd: Boolean = curPos == realPos && lastRead == -1
  }
}

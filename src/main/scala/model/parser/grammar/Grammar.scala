package model.parser.grammar

import Grammar.{EPS, FirstFollowMap, getFirst}
import model.parser.grammar.entry.*
import model.parser.grammar.entry.{GrammarEntry, NonTerminal, Terminal, TranslatingSymbol}
import model.parser.util.Ternary.Ternary

import scala.annotation.tailrec

final case class Grammar[T <: Token](
  name: String,
  headers: Option[TranslatingSymbol],
  parserRules: Map[String, ParserRule],
  lexerRules: List[LexerRule[T]],
  FIRST: FirstFollowMap,
  FOLLOW: FirstFollowMap,
  illegalTokens: Set[String] = Set("EOF")
) {

  def checkLL1(): Unit = {
    for ((ruleName, rule) <- parserRules) {
      val rules = rule.rules.toList
      for (i <- 0 until (rules.length - 1)) {
        for (j <- i + 1 until rules.length) {
          val cur = getNextRule(rules(i), ruleName)
          val temp = getNextRule(rules(j), ruleName)
          if (cur.intersect(temp).nonEmpty) {
            throw new IllegalArgumentException("Not LL1")
          }
        }
      }
    }
  }

  def checkLexerRules(): Unit = {
    val duplicates = lexerRules.groupBy(r => r.token.getName).filter(entry => entry._2.length > 1).keys
    if (duplicates.nonEmpty) {
      throw new IllegalArgumentException(s"Duplicated lexer rule ${duplicates.head}")
    }

    val illegal = lexerRules.map(_.token.getName).filter(illegalTokens.contains)
    if (illegal.nonEmpty) {
      throw new IllegalArgumentException(s"Illegal token \"${illegal.head}\" found")
    }
  }

  def getNextRule(rule: List[GrammarEntry], ruleName: String): Set[String] = {
    val nextElems = getFirst(rule, FIRST)
    if (nextElems.contains(EPS)) {
      nextElems.-(EPS).++(FOLLOW.apply(ruleName))
    } else {
      nextElems
    }
  }
}

object Grammar {
  type FirstFollowMap = Map[String, Set[String]]
  private type GrammarState = (FirstFollowMap, Boolean)
  val EPS: String = ""

  def apply[T <: Token](
    name: String,
    headers: Option[TranslatingSymbol],
    parserRuleList: List[ParserRule],
    lexerRuleList: List[LexerRule[T]]
  ): Grammar[T] = {
    val parserRules = parserRuleList.map(rule => rule.name -> rule).toMap
    val first = generateFirstFollow(parserRules, genFirstRuleFoldFunc)
    val follow = generateFirstFollow(parserRules, genFollowFoldFunc(first))
    val grammar = new Grammar[T](name, headers, parserRules, lexerRuleList, first, follow)
    grammar.checkLexerRules()
    grammar.checkLL1()

    grammar
  }

  @tailrec
  def getFirst(rule: List[GrammarEntry], first: FirstFollowMap): Set[String] =
    rule match {
      case Nil => Set(EPS)
      case x :: xs =>
        x match {
          case term: Terminal       => Set(term.value)
          case _: TranslatingSymbol => getFirst(xs, first)
          case nt: NonTerminal      => first.getOrElse(nt.name, first.getOrElse(nt.value, Set.empty))
        }
    }

  private def generateFirstFollow(
    parserRules: Map[String, ParserRule],
    foldFunc: String => (GrammarState, List[GrammarEntry]) => GrammarState
  ): FirstFollowMap =
    buildFirstOrFollow(parserRules.keys.map(key => key -> Set.empty[String]).toMap, nextIter = true)(
      parserRules,
      foldFunc
    )

  @tailrec
  private def buildFirstOrFollow(entryMap: FirstFollowMap, nextIter: Boolean)(
    parserRules: Map[String, ParserRule],
    foldFunc: String => (GrammarState, List[GrammarEntry]) => GrammarState
  ): FirstFollowMap =
    if (nextIter) {
      val iterRes = parserRules.foldLeft((entryMap, false))((state, parserRule) => {
        val ruleName = parserRule._1
        val rules = parserRule._2
        rules.rules.foldLeft(state)(foldFunc(ruleName))
      })
      buildFirstOrFollow(iterRes._1, iterRes._2)(parserRules, foldFunc)
    } else {
      entryMap
    }

  @tailrec
  private def genFollowFoldFunc(
    first: FirstFollowMap
  )(ruleName: String)(state: GrammarState, rule: List[GrammarEntry]): GrammarState = {
    rule match {
      case Nil => state
      case entry :: xs =>
        entry match {
          case nt: NonTerminal =>
            val oldFirst = state._1.getOrElse(nt.value, Set.empty)
            val newElems = getNewFollowElems(getFirst(xs, first), ruleName)(state._1)
            genFollowFoldFunc(first)(ruleName)(
              newElems.subsetOf(oldFirst) ?? (state, (state._1 + (nt.value -> oldFirst.++(newElems)), true)),
              xs
            )
          case _ => genFollowFoldFunc(first)(ruleName)(state, xs)
        }
    }
  }

  private def getNewFollowElems(newElems: Set[String], ruleName: String)(follow: FirstFollowMap): Set[String] =
    if (newElems.contains(EPS)) {
      val minusEps = newElems.-(EPS)
      minusEps.++(follow.getOrElse(ruleName, Set.empty))
    } else {
      newElems
    }

  private def genFirstRuleFoldFunc(
    ruleName: String
  )(state: GrammarState, rule: List[GrammarEntry]): GrammarState = {
    val curFirst = state._1
    val ruleFirst = extractFirstByRule(rule, curFirst)
    val oldFirst = curFirst.getOrElse(ruleName, Set.empty)
    if (!ruleFirst.subsetOf(oldFirst)) {
      (curFirst + (ruleName -> (oldFirst ++ ruleFirst)), true)
    } else {
      state
    }
  }

  private def extractFirstByRule(rule: List[GrammarEntry], first: FirstFollowMap): Set[String] =
    rule match {
      case Nil => Set(EPS)
      case x :: xs =>
        x match {
          case nt: NonTerminal =>
            first.get(nt.value) match {
              case Some(set) => set ++ (set.contains(EPS) ?? (extractFirstByRule(xs, first), Set.empty))
              case None =>
                first.get(nt.name) match {
                  case Some(value) => value ++ (value.contains(EPS) ?? (extractFirstByRule(xs, first), Set.empty))
                  case None        => Set.empty
                }
            }
          case _: TranslatingSymbol => extractFirstByRule(xs, first)
          case term: Terminal       => Set(term.value)
        }
    }
}

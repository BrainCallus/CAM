package model.parser.grammar

import enumeratum._

import java.util.regex.Pattern
import scala.annotation.unused

trait Token extends EnumEntry { self =>
  protected def pattern: String
  @unused
  def getPattern: Pattern = Pattern.compile(pattern)
  def getPatternAsString: String = pattern
  def getName: String = {
    val name = self.getClass.getSimpleName
    name.substring(0, name.length - 1)
  }
}

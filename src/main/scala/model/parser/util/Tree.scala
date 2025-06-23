package model.parser.util

import cats.Show

import scala.language.implicitConversions

trait Tree[T] {
  implicit def showEv: Show[T]
  def root: T
  def children: List[Tree[?]]
  def showRoot: String = showEv.show(root)

}

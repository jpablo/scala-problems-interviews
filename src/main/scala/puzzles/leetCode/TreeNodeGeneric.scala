package puzzles.leetCode

import java.util.UUID.randomUUID

case class TreeNodeGeneric[A](
    value: A,
    left:  TreeNodeGeneric[A] | Null = null,
    right: TreeNodeGeneric[A] | Null = null
):
  override def toString: String =
    s"[$value, l=${toStr(left)}, r=${toStr(right)}]"

  def toDOT: String =
    s"""digraph G {
       |$toDOTBody
       |}
       |""".stripMargin

  def toDOTBody: String =
    val id = hashCode()
    val leftDot =
      if left != null then
        s"""$id -> ${left.hashCode()} [label=""]\n${left.toDOTBody}"""
      else
        val uuid = randomUUID()
        s""""$uuid" [shape="point"]\n$id -> "$uuid" [label=""]"""
    val rightDot =
      if right != null then
        s"""$id -> ${right.hashCode()} [label=""]\n${right.toDOTBody}"""
      else
        val uuid = randomUUID()
        s""""$uuid" [shape="point"]\n$id -> "$uuid" [label=""]"""

    s"""$id [label="$value"]
       |$leftDot
       |$rightDot
       |""".stripMargin

object TreeNodeGeneric:
  def node[A](
      value: A,
      left:  TreeNodeGeneric[A] | Null = null,
      right: TreeNodeGeneric[A] | Null = null
  ): TreeNodeGeneric[A] =
    TreeNodeGeneric(value, left, right)
end TreeNodeGeneric

def toStr[B](a: B | Null): String = a match
  case null => ""
  case _    => a.toString

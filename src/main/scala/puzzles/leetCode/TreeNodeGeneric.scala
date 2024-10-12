package puzzles.leetCode

case class TreeNodeGeneric[A](
    value: A,
    left:  TreeNodeGeneric[A] | Null = null,
    right: TreeNodeGeneric[A] | Null = null
):
  override def toString: String =
    s"[$value, l=${toStr(left)}, r=${toStr(right)}]"

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

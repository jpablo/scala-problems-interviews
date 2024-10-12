package puzzles.leetCode

class TreeNode(_value: Int = 0, _left: TreeNode | Null = null, _right: TreeNode | Null = null) {
  var value: Int = _value
  var left: TreeNode | Null = _left
  var right: TreeNode | Null = _right

  def toStr[A](a: A | Null): String = a match
    case null => "null"
    case _ => a.toString

  override def toString: String =
    s"TreeNode($value, ${toStr(left)}, ${toStr(right)})"
}



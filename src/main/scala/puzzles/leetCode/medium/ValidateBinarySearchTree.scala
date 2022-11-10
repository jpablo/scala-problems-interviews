package puzzles.leetCode.medium

import puzzles.leetCode.TreeNode


def isValidBST(root: TreeNode): Boolean = {
  def go(t: TreeNode | Null, min: Long, max: Long) : Boolean =
    if (t == null) 
      true
    else {
      min <= t.value && t.value <= max &&
      go(t.left, min, t.value.toLong - 1) && go(t.right, t.value.toLong + 1, max)
    }
  go(root, Int.MinValue, Int.MaxValue)
}
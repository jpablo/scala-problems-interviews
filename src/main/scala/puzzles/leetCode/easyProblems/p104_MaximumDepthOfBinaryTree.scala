package puzzles.leetCode.easyProblems

import scala.annotation.tailrec
//import scala.language.unsafeNulls
import puzzles.leetCode.TreeNodeGeneric.node

// 104
// https://leetcode.com/problems/maximum-depth-of-binary-tree/

// Given the root of a binary tree, return its maximum depth.
//
// A binary tree's maximum depth is the number of nodes along the longest path from the root node down to the farthest leaf node.
//

def maxDepth(root: Tree): Int =
  /** Idea: store the current depth in the stack!
    */
  @tailrec
  def loop(root: Tree, pending: List[(Tree, Int)], rootDepth: Int, maxDepth: Int): Int =
    (root, pending) match
      case (null, Nil)                => maxDepth
      case (null, (right, d) :: tail) => loop(root = right, pending = tail, rootDepth = d, maxDepth = maxDepth)
      case (n: TreeNode, _) =>
        loop(
          root      = n.left,
          pending   = (n.right, rootDepth + 1) :: pending,
          rootDepth = rootDepth + 1,
          maxDepth  = math.max(maxDepth, rootDepth)
        )
  end loop

  if (root == null)
    0
  else
    loop(root, rootDepth = 1, pending = List.empty, maxDepth = 1)

@main def main104(): Unit =
  println(maxDepth(node(3, node(9), node(20, node(15), node(7)))))

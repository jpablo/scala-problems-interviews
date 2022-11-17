package puzzles.leetCode

import scala.annotation.tailrec
import scala.language.unsafeNulls

// 104
// https://leetcode.com/problems/maximum-depth-of-binary-tree/


// Given the root of a binary tree, return its maximum depth.
// 
// A binary tree's maximum depth is the number of nodes along the longest path from the root node down to the farthest leaf node.
// 


def maxDepth(root: TreeNode): Int = {
  
  /**
    * Idea: store the current depth in the stack! 
    */
  @tailrec  
  def loop (root: TreeNode, rootDepth: Int, pending: List[(TreeNode, Int)], maxDepth: Int): Int =
    (root, pending) match {
      case (null, Nil)            => maxDepth
      case (null, (r, d) :: tail) => loop(r, d, tail, maxDepth)
      case _ => 
        loop(
          root      = root.left, 
          rootDepth = rootDepth + 1, 
          pending   = (root.right, rootDepth + 1) :: pending, 
          maxDepth  = math.max(maxDepth, rootDepth)
        )
    }
  if (root == null) 
    0
  else
    loop(root, rootDepth = 1, pending = List.empty, maxDepth = 1)
}

@main def main104 =
  println(maxDepth(TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7)))))
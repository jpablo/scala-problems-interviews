package puzzles.leetCode

import scala.annotation.tailrec
import scala.language.unsafeNulls

def maxDepth(root: TreeNode): Int = {
  
  /**
    * Idea: store the current depth in the stack! 
    */
  @tailrec  
  def go (node: TreeNode, depth: Int, maxDepth: Int, stack: List[(TreeNode, Int)]): Int =
    (node, stack) match {
      case (null, Nil) => 
        maxDepth
      case (null, (r, rDepth) :: tail) => 
        go(r, rDepth, maxDepth, tail)
      case _ => 
        go(node.left, depth + 1, math.max(maxDepth, depth), (node.right, depth + 1) :: stack)
    }
  
  if (root == null) 
    0
  else
    go(root, depth = 1, maxDepth = 1, stack = List.empty)
    
}

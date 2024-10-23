package puzzles.leetCode.easyProblems

import algorithms.preorderLoop

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
      case (n: TreeNode, _) =>
        loop(
          root      = n.left,
          pending   = (n.right, rootDepth + 1) :: pending,
          rootDepth = rootDepth + 1,
          maxDepth  = maxDepth max rootDepth
        )
      case (null, (right, rightDepth) :: tail) => loop(root = right, pending = tail, rootDepth = rightDepth, maxDepth = maxDepth)
      case (null, Nil)                         => maxDepth
  end loop

  if (root == null)
    0
  else
    loop(root, pending = List.empty, rootDepth = 1, maxDepth = 1)

case class Accumulated(rootDepth: Int, maxDepth: Int)
case class Pending(right: Tree, rightDepth: Int)

def maxDepth2(root: Tree): Int =
  if (root == null)
    0
  else
    preorderLoop(
      root          = root,
      pending       = List.empty[Pending],
      acc           = Accumulated(rootDepth = 1, maxDepth = 1),
      nextPending   = (n, acc) => Pending(right = n.right, rightDepth = acc.rootDepth + 1),
      nextAcc       = (_, acc) => Accumulated(rootDepth = acc.rootDepth + 1, maxDepth = acc.maxDepth max acc.rootDepth),
      backtrackRoot = _.right,
      backtrackAcc  = (p, acc) => Accumulated(rootDepth = p.rightDepth, maxDepth = acc.maxDepth)
    ).maxDepth

@main def main104(): Unit =
  println(maxDepth(node(3, node(9), node(20, node(15), node(7)))))
  println(maxDepth2(node(3, node(9), node(20, node(15), node(7)))))

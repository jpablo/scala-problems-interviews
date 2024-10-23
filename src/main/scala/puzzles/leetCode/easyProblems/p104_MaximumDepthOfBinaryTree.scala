package puzzles.leetCode.easyProblems

import algorithms.{PreorderData, preorder}

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

// ---------------------------------

case class Accumulated(rootDepth: Int, maxDepth: Int):
  def next =
    Accumulated(rootDepth = rootDepth + 1, maxDepth = maxDepth max rootDepth)

type Pending = (right: Tree, rightDepth: Int)

def maxDepth2(root: Tree): Int =
  val data = PreorderData[Char, Accumulated, Pending](
    nextPending   = (n, acc) => (right = n.right, rightDepth = acc.rootDepth + 1),
    accumulate    = (_, acc) => acc.next,
    backtrackRoot = p => p.right,
    backtrackAcc  = (p, acc) => acc.copy(rootDepth = p.rightDepth)
  )

  if (root == null)
    0
  else
    preorder(root, pending = List.empty, Accumulated(1, 1), data).maxDepth

@main def main104(): Unit =
  println(maxDepth(node(3, node(9), node(20, node(15), node(7)))))
  println(maxDepth2(node(3, node(9), node(20, node(15), node(7)))))

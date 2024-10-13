package puzzles.leetCode.easyProblems

import puzzles.leetCode.TreeNodeGeneric
import puzzles.leetCode.TreeNodeGeneric.node

import scala.annotation.tailrec

// 94
// https://leetcode.com/problems/binary-tree-inorder-traversal/

/*
Given the root of a binary tree, return the inorder traversal of its nodes' values.
 */

type TreeNode = TreeNodeGeneric[Char]
type Tree = TreeNode | Null

/** preorder: root, left, right
  *
  * Idea:
  *   - Stack of pending nodes
  *   - use value when visiting node
  */
def preorderTraversal(root: TreeNode) =
  // pending = right branches
  @tailrec
  def loop[B](root: Tree, pending: List[Tree], acc: B, f: (Char, B) => B): B =
    (root, pending) match
      case (node: TreeNode, _) => loop(root = node.left, pending = node.right :: pending, acc = f(node.value, acc), f)
      case (null, right :: rr) => loop(root = right, pending = rr, acc, f)
      case (null, Nil)         => acc

  loop(root, Nil, Nil, _ :: _).reverse

/** inorder: left, root, right
  *
  * Idea:
  *   - Stack of pending nodes + value
  *   - use value when taking a node from the stack
  */
def inorderTraversal(root: TreeNode): List[Char] =
  @tailrec
  def loop[B](root: Tree, pending: List[(Char, Tree)], acc: B, f: (Char, B) => B): B =
    (root, pending) match
      case (n: TreeNode, _)             => loop(root = n.left, pending = (n.value, n.right) :: pending, acc, f)
      case (null, (value, right) :: rr) => loop(root = right, pending = rr, acc = f(value, acc), f)
      case (null, Nil)                  => acc

  loop(root, Nil, Nil, _ :: _).reverse

/** postorder: left, right, root Idea:
  *   - Stack of pending nodes | value
  *   - use value when taking a node from the stack
  */
def postorderTraversal(root: TreeNode): List[Char] = {
  @tailrec
  def loop[B](root: Tree, pending: List[Char | Tree], acc: B, f: (Char, B) => B): B =
    (root, pending) match
      case (n: TreeNode, _) => loop(n.left, n.right :: n.value :: pending, acc, f)

      case (null, rightOrValue :: rr) =>
        rightOrValue match
          case value: Char     => loop(null, rr, f(value, acc), f)
          case right: TreeNode => loop(root = right, rr, acc, f)
          case null            => loop(null, rr, acc, f)

      case (null, Nil) => acc

  loop(root, Nil, Nil, _ :: _).reverse
}

@main def main94(): Unit =
  val t = node('a', node('b', node('d'), node('e')), node('c', node('f')))
//  val result = preorderTraversal(t)
  val result = inorderTraversal(t)
  println("------------------")
  println(result)
//  println(inorderTraversal(t))
//  println(postorderTraversal(t))

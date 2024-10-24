package puzzles.leetCode.easyProblems

import algorithms.{PreorderData, preorder}
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
  *   - Process each node before moving to the next
  */
def preorderTraversal(root: TreeNode) =
  val data = PreorderData[Char, List[Char], Tree](
    nextPending   = (n, _) => n.right,
    accumulate    = (n, acc) => n.value :: acc,
    backtrackRoot = r => r,
    backtrackAcc  = (_, acc) => acc
  )
  preorder(root, pending = List.empty, acc = List.empty, data).reverse

def preorderTraversal2(root: TreeNode) =
  type Pending = Tree
  @tailrec
  def preorderLoop[B](root: Tree, rightStack: List[Pending], acc: B, f: (Char, B) => B): B =
    (root, rightStack) match
      // 1. process node
      case (n: TreeNode, _)      => preorderLoop(root = n.left, rightStack = n.right :: rightStack, acc = f(n.value, acc), f)
      // 2. backtracking
      case (null, right :: tail) => preorderLoop(root = right, rightStack = tail, acc, f)
      // 3. done
      case (null, Nil)           => acc

  preorderLoop(root, rightStack = Nil, acc = Nil, f = (value, acc) => value :: acc).reverse

/** inorder: left, root, right
  *
  * Idea:
  *   - Stack of pending nodes + value
  *   - use value when taking a node from the stack
  */
def inorderTraversal(root: TreeNode): List[Char] =
  type Pending = (Tree, Char)
  @tailrec
  def inorderLoop[B](root: Tree, pending: List[Pending], acc: B, f: (Char, B) => B): B =
    (root, pending) match
      case (n: TreeNode, _)               => inorderLoop(root = n.left, pending = (n.right, n.value) :: pending, acc, f)
      case (null, (right, value) :: tail) => inorderLoop(root = right, pending = tail, acc = f(value, acc), f)
      case (null, Nil)                    => acc

  @tailrec
  def postOrderLoop[B](root: Tree, pending: List[Tree | Char], acc: B, f: (Char, B) => B): B =
    (root, pending) match
      // 1. process node
      case (n: TreeNode, _) => postOrderLoop(root = n.left, pending = n.value :: n.right :: pending, acc, f)
      // 2. backtracking
      case (null, p :: tail) =>
        p match
          case value: Char     => postOrderLoop(root = null, pending = tail, acc = f(value, acc), f)
          case right: TreeNode => postOrderLoop(root = right, pending = tail, acc, f)
          case null            => postOrderLoop(root = null, pending = tail, acc, f)
      // 3. done
      case (null, Nil) => acc

  inorderLoop(root, pending = Nil, acc = Nil, f = _ :: _).reverse

/** postorder: left, right, root
  *
  * Idea:
  *   - Stack of pending nodes | value
  *   - use value when taking a node from the stack
  */
def postorderTraversal(root: TreeNode): List[Char] =
  type Pending = Tree | Char

  @tailrec
  def postOrderLoop[B](root: Tree, pending: List[Pending], acc: B, f: (Char, B) => B): B =
    (root, pending) match
      case (null, Nil) => acc
      case (null, p :: tail) =>
        p match
          case right: TreeNode => postOrderLoop(root = right, tail, acc, f)
          case value: Char     => postOrderLoop(null, tail, f(value, acc), f)
          case null            => postOrderLoop(null, tail, acc, f)

      case (n: TreeNode, _) => postOrderLoop(root = n.left, pending = n.right :: n.value :: pending, acc, f)

  postOrderLoop(root, Nil, Nil, _ :: _).reverse

@main def main94(): Unit =
  val t = node('a', left = node('b', node('d'), node('e')), right = node('c', node('f')))
  val t2 =
    node(
      value = '1',
      left  = node('2', node('3'), node('4')),
      right = node('2', node('4'), node('3'))
    )
//  println(t.toDOT)
  println(preorderTraversal(t))
  println(preorderTraversal2(t))
//  val result = inorderTraversal(t)
//  val result = postorderTraversal(t)
  println("------------------")
//  println(result)
//  println("pre")
//  println(preorderTraversal(t2.left.nn))
//  println(preorderTraversal(t2.right.nn))
//  println("in")
//  println(inorderTraversal(t2.left.nn))
//  println(inorderTraversal(t2.right.nn))
//  println("post")
//  println(postorderTraversal(t2.left.nn))
//  println(postorderTraversal(t2.right.nn))

package puzzles.leetCode

import scala.annotation.tailrec

// 94
// https://leetcode.com/problems/binary-tree-inorder-traversal/

/*
Given the root of a binary tree, return the inorder traversal of its nodes' values.
*/

/**
  * preorder: root, left, right
  * 
  * Idea:
    - Stack of pending nodes
    - use value when visiting node
  */
def preorderTraversal(root: TreeNode): List[Int] = {
  @tailrec
  def loop[B](root: TreeNode | Null, pending: List[TreeNode | Null], result: B, use: (Int, B) => B): B =
    (root, pending) match {
      case (n: TreeNode, _) => loop(n.left, n.right :: pending, use(n.value, result), use)
      case (null, r :: t)   => loop(r, t, result, use)
      case (null, Nil   )   => result
    }

  val values = loop(root, List.empty, List.empty[Int], _ :: _)
  values.reverse
}


/**
  * inorder: left, root, right
  *
  * Idea:
  *  - Stack of pending nodes + value
  */
def inorderTraversal(root: TreeNode): List[Int] = {
  @tailrec
  def loop[B](root: TreeNode | Null, pending: List[(Int, TreeNode | Null)], result: B, use: (Int, B) => B): B =
    (root, pending) match {
      case (n: TreeNode, _)        => loop(n.left, (n.value, n.right) :: pending, result, use)
      case (null, (value, r) :: t) => loop(r, t, use(value, result), use)
      case (null, Nil)             => result
    }

  val values = loop(root, List.empty, List.empty[Int],  _ :: _)
  values.reverse
}



/**
  * postorder: left, right, root
  */
def postorderTraversal(root: TreeNode): List[Int] = {
  @tailrec
  def loop[B](root: TreeNode | Null, pending: List[Int | TreeNode | Null], result: B, use: (Int, B) => B): B =
    (root, pending) match {
      case (n: TreeNode, _) => 
        loop(n.left, n.right :: n.value :: pending, result, use)

      case (null, h :: t) => 
        h match 
          case value: Int  => loop(null, t, use(value, result), use)
          case r: TreeNode => loop(r   , t, result,             use)
          case null        => loop(null, t, result,             use)
        
      case (null, Nil) => 
        result
    }

  val values = loop(root, List.empty, List.empty[Int],  _ :: _)
  values.reverse
}

@main def main94 =
  val t = TreeNode(1, null, TreeNode(2, TreeNode(3)))
  println(preorderTraversal(t))
  println(inorderTraversal(t))
  println(postorderTraversal(t))


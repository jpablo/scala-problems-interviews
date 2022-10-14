package puzzles.leetCode

import scala.annotation.tailrec



// Definition for a binary tree node.

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}


/**
  * preorder: root, left, right
  */
def preorderTraversal(root: TreeNode): List[Int] = {

  @tailrec
  def go(root: TreeNode, acc: List[Int], stack: List[TreeNode]): List[Int] =
    (root, stack) match {
      case (null, Nil   ) => acc                  // (a) end of tree
      case (null, r :: t) => go(r, acc, t)        // (b) leaf node
      case _              => go(root.left, root.value :: acc, root.right :: stack) // (c) branch node: read and use value
    }

  val values = go(root, List.empty, List.empty)
  values.reverse
}


/**
  * inorder: left, root, right
  */
def inorderTraversal(root: TreeNode): List[Int] = {

  @tailrec
  def go(root: TreeNode, acc: List[Int], stack: List[(Int, TreeNode)]): List[Int] =
    (root, stack) match {
      case (null, Nil)             => acc  // (a)
      case (null, (value, r) :: t) => go(r, value :: acc, t)  // (b) leaf node: use value
      case _                       => go(root.left, acc, (root.value, root.right) :: stack) // (c) branch: read value (and store it)
    }
    
  val values = go(root, List.empty, List.empty)
  values.reverse
}



/**
  * postorder: left, right, root
  */
def postorderTraversal(root: TreeNode): List[Int] = {


  @tailrec
  def go(root: TreeNode, acc: List[Int], stack: List[(Int, TreeNode)]): List[Int] =
    (root, stack) match {
      case (null, Nil)             => acc  // (a)
      case (null, (value, r) :: t) => go(r, value :: acc, t)  // (b) leaf node: use value
      case _                       => go(root.left, acc, (root.value, root.right) :: stack) // (c) branch: read value (and store it)
    }
    
  val values = go(root, List.empty, List.empty)
  values
}
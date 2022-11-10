package puzzles.leetCode

import scala.annotation.tailrec



/**
  * preorder: root, left, right
  */
def preorderTraversal(root: TreeNode): List[Int] = {

  @tailrec
  def go(root: TreeNode | Null, acc: List[Int], stack: List[TreeNode | Null]): List[Int] =
    (root, stack) match {
      case (null, Nil   )        => acc                  // (a) end of tree
      case (null, r :: t)        => go(r, acc, t)        // (b) leaf node
      case (nnRoot: TreeNode, _) => go(nnRoot.left, nnRoot.value :: acc, nnRoot.right :: stack) // (c) branch node: read and use value
    }

  val values = go(root, List.empty, List.empty)
  values.reverse
}


/**
  * inorder: left, root, right
  */
def inorderTraversal(root: TreeNode): List[Int] = {

  @tailrec
  def go(root: TreeNode | Null, acc: List[Int], stack: List[(Int, TreeNode | Null)]): List[Int] =
    (root, stack) match {
      case (null, Nil)             => acc  // (a)
      case (null, (value, r) :: t) => go(r, value :: acc, t)  // (b) leaf node: use value
      case (nnRoot: TreeNode, _)   => go(nnRoot.left, acc, (nnRoot.value, nnRoot.right) :: stack) // (c) branch: read value (and store it)
    }
    
  val values = go(root, List.empty, List.empty)
  values.reverse
}



/**
  * postorder: left, right, root
  */
def postorderTraversal(root: TreeNode): List[Int] = {
  ???
}



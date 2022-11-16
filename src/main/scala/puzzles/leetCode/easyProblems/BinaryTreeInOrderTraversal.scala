package puzzles.leetCode

import scala.annotation.tailrec



/**
  * preorder: root, left, right
  */
def preorderTraversal(root: TreeNode): List[Int] = {

  @tailrec
  def loop[B](root: TreeNode | Null, pending: List[TreeNode | Null], result: B, f: (Int, B) => B): B =
    (root, pending) match {
      case (null, Nil   )        => result                  // (a) end of tree
      case (null, r :: t)        => loop(r, t, result, f)      // (b) leaf node
      case (nnRoot: TreeNode, _) => loop(nnRoot.left, nnRoot.right :: pending, f(nnRoot.value, result), f) // (c) branch node: read and use value
    }

  val values = loop(root, List.empty, List.empty[Int], _ :: _)
  values.reverse
}


/**
  * inorder: left, root, right
  */
def inorderTraversal(root: TreeNode): List[Int] = {

  @tailrec
  def loop(root: TreeNode | Null, pending: List[(Int, TreeNode | Null)], result: List[Int]): List[Int] =
    (root, pending) match {
      case (null, Nil)             => result  // (a)
      case (null, (value, r) :: t) => loop(r, t, value :: result)  // (b) leaf node: use value
      case (nnRoot: TreeNode, _)   => loop(nnRoot.left, (nnRoot.value, nnRoot.right) :: pending, result) // (c) branch: read value (and store it)
    }
    
  val values = loop(root, List.empty, List.empty)
  values.reverse
}



/**
  * postorder: left, right, root
  */
def postorderTraversal(root: TreeNode): List[Int] = {
  ???
}



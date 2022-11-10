package puzzles.leetCode.medium

import puzzles.leetCode.TreeNode

// - Identify root node (preorder)
// - Identify left, right subrees (using InOrder)
def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode | Null = {
  if (preorder.isEmpty || inorder.isEmpty)
    null
  else {
    val root = preorder(0)
    val (leftIn, rightIn0) = inorder.splitAt(inorder.indexOf(root))
    val rightIn = rightIn0.drop(1)
    assert(preorder.size == 1 + leftIn.size + rightIn.size )
    val leftPre = preorder.drop(1).take(leftIn.size)
    val rightPre = preorder.drop(1 + leftIn.size)
    val leftTree = buildTree(leftPre, leftIn)
    val rightTree = buildTree(rightPre, rightIn)
    TreeNode(root, leftTree, rightTree)
  }
}

def buildTree2(preorder: Array[Int], inorder: Array[Int]): TreeNode | Null = {

  def go(preStart: Int, preEnd: Int, inStart: Int, inEnd: Int): TreeNode | Null = {
    val root = preorder(0)
    val rootI = inorder.indexOf(root, inStart)
    

    ???
  }

  if (preorder.isEmpty || inorder.isEmpty)
    null
  else {
    val root = preorder(0)
    val (leftIn, rightIn0) = inorder.splitAt(inorder.indexOf(root))
    val rightIn = rightIn0.drop(1)
    assert(preorder.size == 1 + leftIn.size + rightIn.size )
    val leftPre = preorder.drop(1).take(leftIn.size)
    val rightPre = preorder.drop(1 + leftIn.size)
    TreeNode(root, 
      buildTree2(leftPre, leftIn), 
      buildTree2(rightPre, rightIn)
    )
  }
}

@main def mainBTfromPreorder =

  val t1 = TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7)))
  val t1_ = buildTree(Array(3,9,20,15,7), Array(9,3,15,20,7))
  val t2 = TreeNode(-1)
  val t2_ = buildTree(Array(-1), Array(-1))
  println(t1_)
  println(t2_)
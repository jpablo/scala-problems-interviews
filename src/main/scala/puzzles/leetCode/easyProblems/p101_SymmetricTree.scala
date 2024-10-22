package puzzles.leetCode.easyProblems

import scala.collection.mutable
import scala.language.unsafeNulls

def isSymmetric(root: TreeNode): Boolean =
  root == null || isMirror(root.left, root.right)

def isMirror(left: TreeNode, right: TreeNode): Boolean =
  (left, right) match
    case (null, null)                   => true
    case (null, _)                      => false
    case (_, null)                      => false
    case _ if left.value != right.value => false
    case _                              => isMirror(left.left, right.right) && isMirror(left.right, right.left)

def isSymmetric2(root: TreeNode): Boolean =
  // duplicate root
  val q = mutable.Queue((root, root))
  while q.nonEmpty do
    val (left, right) = q.dequeue
    (left, right) match
      case (null, null)                   => ()
      case (null, _)                      => return false
      case (_, null)                      => return false
      case _ if left.value != right.value => return false
      case _                              => q.enqueueAll(List(left.left -> right.right, left.right -> right.left))
  true

val example1 =
  new TreeNode(1, new TreeNode(2, new TreeNode(3), new TreeNode(4)), new TreeNode(2, new TreeNode(4), new TreeNode(3)))

val example2 =
  new TreeNode(1, new TreeNode(2, null, new TreeNode(3)), new TreeNode(2, null, new TreeNode(3)))

@main def run1(): Unit =
  println(isSymmetric2(example1))
  println(isSymmetric2(example2))

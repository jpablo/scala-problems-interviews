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
  val q = mutable.Queue.empty[TreeNode]
  q.enqueue(root)
  q.enqueue(root)
  while q.nonEmpty do
    val t1 = q.dequeue // [t1.left]
    val t2 = q.dequeue // [t2.right]
    if t1 == null && t2 == null then ()
    else if t1 == null || t2 == null then return false
    else if t1.value != t2.value then return false
    else // q ++ [t1.left, t2.right, t1.right, t2.left]
      q.enqueueAll(List(t1.left, t2.right, t1.right, t2.left))
  true

val example1 =
  new TreeNode(1, new TreeNode(2, new TreeNode(3), new TreeNode(4)), new TreeNode(2, new TreeNode(4), new TreeNode(3)))

val example2 =
  new TreeNode(1, new TreeNode(2, null, new TreeNode(3)), new TreeNode(2, null, new TreeNode(3)))

@main def run1(): Unit =
  println(isSymmetric2(example1))
  println(isSymmetric2(example2))

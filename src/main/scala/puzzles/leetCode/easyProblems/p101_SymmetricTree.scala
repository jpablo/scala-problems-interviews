package puzzles.leetCode.easyProblems

import scala.collection.{immutable, mutable}
import scala.language.unsafeNulls
import puzzles.leetCode.TreeNodeGeneric
import puzzles.leetCode.TreeNodeGeneric.node

import scala.annotation.tailrec

def isSymmetric[A](root: TreeNodeGeneric[A]): Boolean =
  root == null || isMirrorRec(immutable.Queue(root.left -> root.right))

def isMirror(left: TreeNode, right: TreeNode): Boolean =
  (left, right) match
    case (null, null)                   => true
    case (null, _)                      => false
    case (_, null)                      => false
    case _ if left.value != right.value => false
    case _                              => isMirror(left.left, right.right) && isMirror(left.right, right.left)

@tailrec
def isMirrorRec[A](pairs: immutable.Queue[(TreeNodeGeneric[A], TreeNodeGeneric[A])]): Boolean =
  if pairs.isEmpty then
    true
  else
    val ((left, right), tail) = pairs.dequeue
    (left, right) match
      case (null, null)                   => isMirrorRec(tail)
      case (null, _)                      => false
      case (_, null)                      => false
      case _ if left.value != right.value => false
      case _                              => isMirrorRec(tail.enqueueAll(List(left.left -> right.right, left.right -> right.left)))

def isSymmetric2[A](root: TreeNodeGeneric[A]): Boolean =
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
  node(1, node(2, node(3), node(4)), node(2, node(4), node(3)))

val example2 =
  node(1, node(2, null, node(3)), node(2, null, node(3)))

@main def run1(): Unit =
  println(isSymmetric(example1))
//  println(isSymmetric(example2))

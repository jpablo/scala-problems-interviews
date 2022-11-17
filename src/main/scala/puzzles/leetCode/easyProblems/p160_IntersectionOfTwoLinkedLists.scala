package puzzles.leetCode.easyProblems

import scala.annotation.tailrec

import scala.language.unsafeNulls

// 1. Go to the last node in both lists
// 2. go backward one at ah time in both lists
// 3  The moment the elements are different, we found the divergence

// Note: better idea: use two pointer that go around the end to make them start
// at same distance from the end
// https://leetcode.com/problems/intersection-of-two-linked-lists/

def getIntersectionNode0(headA: ListNode, headB: ListNode): ListNode = {

  def length(node: ListNode, acc: Int = 0): Int =
    if (node == null) acc else length(node.next, acc + 1)
  
  def drop(node: ListNode, n: Int): ListNode =
    if (node == null || n == 0) node else drop(node.next, n - 1)
  
  val lengthA = length(headA)
  val lengthB = length(headB)

  var (h1, h2) =
    if (lengthA > lengthB)
      (drop(headA, lengthA - lengthB), headB)
    else
      (drop(headB, lengthB - lengthA), headA)
  while (h1 != h2) {
    h1 = h1.next
    h2 = h2.next
  }
  h1

}


def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
  var pa = headA
  var pb = headB
  while (pa != pb) {
    println((pa, pb))
    pa = if (pa == null) headB else pa.next
    pb = if (pb == null) headA else pb.next
  }
  pa
}

val listC = ListNode(8, ListNode(4, ListNode(5)))
val listA = ListNode(4, ListNode(1, listC))
val listB = ListNode(5, ListNode(6, ListNode(1, listC)))

val listOne = ListNode(1)

@main def mainIntersection =
  // println(getIntersectionNode(listA, listB))
  // println(getIntersectionNode(listOne, listOne))
  val list3 = ListNode(3)
  println(getIntersectionNode(ListNode(2, list3), list3))
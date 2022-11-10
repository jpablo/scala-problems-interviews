package puzzles.leetCode

import scala.annotation.tailrec

import scala.language.unsafeNulls

// 1. Go to the last node in both lists
// 2. go backward one at ah time in both lists
// 3  The moment the elements are different, we found the divergence

// Note: better idea: use two pointer that go around the end to make them start
// at same distance from the end
// https://leetcode.com/problems/intersection-of-two-linked-lists/

def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
  val listA: List[ListNode] = List.unfold(headA)(n => Option(n).map(nn => (nn, nn.next))).reverse
  val listB: List[ListNode] = List.unfold(headB)(n => Option(n).map(nn => (nn, nn.next))).reverse

  @tailrec
  def go (as: List[ListNode], bs: List[ListNode]): ListNode =
    (as, bs) match {
      case (ah :: ath :: att, bh :: bth :: btt) if (ah, ath) == (bh, bth) => go(ath :: att, bth :: btt)
      case (ah :: _         , bh :: _         ) if ah == bh               => ah
      case _ => null
    }

  go(listA, listB)
}


// listA =   [4,1,8,4,5], 
// listB = [5,6,1,8,4,5], 
val listC = ListNode(8, ListNode(4, ListNode(5)))
val listA = ListNode(4, ListNode(1, listC))
val listB = ListNode(5, ListNode(6, ListNode(1, listC)))


val listOne = ListNode(1)
@main def mainIntersection =
  println(getIntersectionNode(listA, listB))
  println(getIntersectionNode(listOne, listOne))
  val list3 = ListNode(3)
  println(getIntersectionNode(ListNode(2, list3), list3))
package puzzles.leetCode.medium

import scala.language.unsafeNulls

// 148
// https://leetcode.com/problems/sort-list/

def split(head: ListNode): (ListNode, ListNode) = {
  def loop(s: ListNode, f: ListNode, acc: ListNode): ListNode = 
    if (f == null || f.next == null) 
      s
      else {
        acc.next = ListNode(s.x)
        loop(s.next, f.next.next, acc.next)
      }
      
  val dummy = ListNode(0)
  val right = loop(head, head, dummy)
  (dummy.next, right)
}

def merge0(a: ListNode, b: ListNode): ListNode =
  (a, b) match {
    case (null, b) => b
    case (a, null) => a
    case (a, b) if a.x < b.x =>
      a.next = merge0(a.next, b)
      a
    case _ => 
      b.next = merge0(b.next, a)
      b
  }


// using a variation of merge0 sort
def sortList(head: ListNode): ListNode = {
  if (head == null || head.next == null)
    head
  else {
    val (left, right) = split(head)
    merge0(sortList(left), sortList(right))
  }
}

@main def main148 =

  println(sortList(ListNode(4, ListNode(2, ListNode(1, ListNode(3))))))
  println(sortList(ListNode(4, ListNode(2, ListNode(1)))))
  println(sortList(ListNode(4, ListNode(2))))
  println(sortList(ListNode(4)))
  println(sortList(null))

  // println(split(ListNode(1, ListNode(2, ListNode(3, ListNode(4))))))
  // println(split(ListNode(1, ListNode(2, ListNode(3)))))
  // println(split(ListNode(1, ListNode(2))))
  // println(split(ListNode(1)))
  // println(split(null))
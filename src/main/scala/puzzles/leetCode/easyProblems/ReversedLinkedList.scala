package puzzles.leetCode.easyProblems

import scala.annotation.tailrec
import scala.language.unsafeNulls

def reverseList0(head: ListNode): ListNode = {
  def go(head: ListNode, acc: ListNode): ListNode =
    if (head == null)
      acc
    else
      go(head = head.next, acc = ListNode(head.x, acc))
      
  go(head, null)
}

def reverseList1(head0: ListNode): ListNode =
  var head: ListNode = head0
  var acc: ListNode = null
  while (head != null) {
    var next = head.next
    head.next = acc
    acc = head
    head = next
  }
  acc

def reverseList(head: ListNode): ListNode =
  def go(head: ListNode, acc: ListNode): ListNode =
    if (head == null)
      acc
    else
      val next = head.next
      head.next = acc
      go(head = next, acc = head)
  go(head, null)


@main def mainReversedLL =
  val example =
    List(1,2,3).foldLeft(List.empty[Int])((acc, i) => i :: acc)
  
  println(reverseList(null))
  println(reverseList(ListNode(1)))
  println(reverseList(ListNode(1, ListNode(2, ListNode(3)))))

  // reverseList(ListNode(1, ListNode(2, ListNode(3))))

  // go( ListNode(1, ListNode(2, ListNode(3))), null                                        )
  // go( ListNode(2, ListNode(3))             , ListNode(1, null)                           ) 
  // go( ListNode(3)                          , ListNode(2, ListNode(1, null))              )
  // go( null                                 , ListNode(3, ListNode(2, ListNode(1, null))) )

  // ListNode(3, ListNode(2, ListNode(1, null)))
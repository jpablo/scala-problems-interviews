package puzzles.leetCode

import scala.language.unsafeNulls

 class ListNode(
  var x   : Int = 0, 
  var next: ListNode = null
):
  //  var next: ListNode = _next
  //  var x: Int = _x

   override def toString =
    s"ListNode($x, $next)"



object Solution {
    def reverse(lst: ListNode, acc: ListNode = null): ListNode = {
      if (lst == null)
        acc
      else
        reverse(lst.next, new ListNode(lst.x, acc))
    }
  
    def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
      def go(l1: ListNode, l2: ListNode, acc: ListNode = null): ListNode = {
        if (l1 == null && l2 == null)
          acc
        else if (l1 == null)
          go(null, l2.next, new ListNode(l2.x, acc))
        else if (l2 == null)
          go(null, l1.next, new ListNode(l1.x, acc))
        else 
          if (l1.x <= l2.x)
            go(l1.next, l2, new ListNode(l1.x, acc))
          else
            go(l2.next, l1, new ListNode(l2.x, acc))
      }
      reverse(go(list1, list2))
    }
}


def mergeTwoLists2(l1: ListNode, l2: ListNode): ListNode = {
  if (l1 == null)
    l2
  else if (l2 == null)
    l1
  else 
    if (l1.x < l2.x) {
      l1.next = mergeTwoLists2(l1.next, l2)
      l1
    } else {
      l2.next = mergeTwoLists2(l2.next, l1)
      l2
    }
}
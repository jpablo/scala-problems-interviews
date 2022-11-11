package puzzles.leetCode.easyProblems

import puzzles.leetCode.medium.ListNode
import scala.language.unsafeNulls

// 876
// https://leetcode.com/problems/middle-of-the-linked-list/description/


def middleNode(head: ListNode): ListNode = {
  def loop(s: ListNode, f: ListNode): ListNode = 
    if (f == null || f.next == null) s else loop(s.next, f.next.next)
  loop(head, head)
}


@main def main876 =
  val n1 = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
  val n2 = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5, ListNode(6))))))
  println(middleNode(n1))
  println(middleNode(n2))
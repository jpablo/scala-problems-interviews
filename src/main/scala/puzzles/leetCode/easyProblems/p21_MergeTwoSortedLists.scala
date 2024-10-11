package puzzles.leetCode.easyProblems

import upickle.default.writeJs

import scala.language.unsafeNulls

// 21
// https://leetcode.com/problems/merge-two-sorted-lists/

// This is given in the problem description
class ListNode(
    var x:    Int = 0,
    var next: ListNode = null
):
  //  var next: ListNode = _next
  //  var x: Int = _x

  override def toString =
    s"ListNode($x, $next)"

  def toList: List[Int] =
    x :: (if next == null then Nil else next.toList)

def mergeTwoLists(a: ListNode, b: ListNode): ListNode =
  (a, b) match
    case (null, b) => b
    case (a, null) => a
    case _ =>
      if a.x < b.x then
        a.next = mergeTwoLists(a.next, b)
        a
      else
        b.next = mergeTwoLists(b.next, a)
        b

@main def main21(): Unit =
  val n1 = ListNode(1, ListNode(2, ListNode(4)))
  val n2 = ListNode(1, ListNode(3, ListNode(4)))
  println(writeJs(mergeTwoLists(n1, n2).toList))

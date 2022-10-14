package puzzles.leetCode.medium

import scala.annotation.tailrec

class ListNode(
  var x   : Int = 0, 
  var next: ListNode = null
):
   override def toString = s"ListNode($x, $next)"


// 1. ListNode -> String -> BigInt
// 2. BigInt + BigInt
// 2. BigInt -> String -> ListNode 
object Solution {
  
  @tailrec
  def toNumber(l: ListNode, s: String = ""): BigInt = 
    if (l == null) BigInt(s) else toNumber(l.next, l.x.toString + s)
  
  @tailrec
  def toListNode(n: String, l: ListNode): ListNode =
    if (n.isEmpty) 
      l
    else
      toListNode(
        n = n.tail,
        l = new ListNode(n(0).toString.toInt, l)
      )
    
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val n = toNumber(l1) + toNumber(l2)
    toListNode(n.toString, null)
  }
}

// Trick: Normalize representation (next, value) to keep the same loop even if a list is null
object Solution2 {  
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def next (n: ListNode) = if (n != null) n.next else null
    def value(n: ListNode) = if (n != null) n.x    else 0
    @tailrec
    def go(n1: ListNode, n2: ListNode, last: ListNode, c: Int = 0): Unit =
      if (n1 != null || n2 != null || c != 0) {
        val sum = value(n1) + value(n2) + c
        last.next = new ListNode(sum % 10)
        go(next(n1), next(n2), last.next, sum / 10)
      }
    val dummy = new ListNode(0)
    go(l1, l2, dummy)
    dummy.next
  }
}
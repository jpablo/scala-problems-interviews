package puzzles.leetCode.easyProblems

import language.unsafeNulls

// https://leetcode.com/problems/linked-list-cycle/


// Given head, the head of a linked list, determine if the linked list has a cycle in it.
// There is a cycle in a linked list if there is some node in the list that can be reached again by continuously following the next pointer. 
// Internally, pos is used to denote the index of the node that tail's next pointer is connected to. Note that pos is not passed as a parameter.
// Return true if there is a cycle in the linked list. Otherwise, return false.

/* 
Idea
 - Keep track of visited nodes
*/

def hasCycle0(head: ListNode): Boolean = {
  var visited = Set.empty[ListNode]
  var cycle  = false
  var current: ListNode | Null = head
  while (current != null && !cycle) {
    if (visited contains current)
      cycle = true
    else {
      visited += current
      current = current.next
    }          
  }
  cycle        
}

def hasCycle(head: ListNode): Boolean = {
  def loop(head: ListNode, fast: ListNode): Boolean =
    if (fast == null || fast.next == null) 
      false
    else if (head == fast) 
      true
    else 
      loop(head.next, fast.next.next)

  if (head == null) false else loop(head, head.next)
}

@main def main141 =
  val n4 = ListNode(-4)
  val n2 = ListNode(2, ListNode(0, n4))
  n4.next = n2
  println(hasCycle(ListNode(3, n2)))
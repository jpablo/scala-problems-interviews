package puzzles.leetCode.medium

package medium128

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

// #138. Copy List with Random Pointer
// https://leetcode.com/problems/copy-list-with-random-pointer/


class Node(var _value: Int) {
  var value: Int = _value
  var next: Node | Null = null
  var random: Node | Null = null
}



import scala.language.unsafeNulls

def copyRandomList(head: Node): Node = {
  val visited = Map.empty[Node, Node]
  def go(head: Node): Node =
    if (head == null) null
    else if (visited.contains(head))
      visited(head)
    else  {
      val clone = new Node(head.value)
      visited += (head -> clone)
      clone.next   = go(head.next)
      clone.random = go(head.random)
      clone
    }
  go(head)
}



@main def main138 =
  val nodes = Array(
    new Node(7), new Node(13), new Node(11), new Node(10), new Node(1)
  )
  // (0 until nodes.length - 1).foreach(i => nodes(i).next = nodes(i + 1))
  nodes(0).next = nodes(1)
  nodes(1).next = nodes(2)
  nodes(2).next = nodes(3)
  nodes(3).next = nodes(4)

  nodes(1).random = nodes(0)
  nodes(2).random = nodes(4)
  nodes(3).random = nodes(2)
  nodes(4).random = nodes(0)

  // println(toArray(nodes(0)).map(n => (n._value)))

  // println(toArray(copyRandomList(nodes(0))).map(n => (n._value)))
  

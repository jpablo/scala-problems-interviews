package puzzles.leetCode.medium

import collection.mutable.Queue

class Node(var _value: Int):
  var value: Int = _value
  var left : Node | Null = null
  var right: Node | Null = null
  var next : Node | Null = null
  
  override def toString(): String =
    Node.toString(this)

object Node:
  def toString(n: Node | Null): String =
    if (n == null) "null"
    else
      s"Node(${n.value}, left = ${toString(n.left)}, right = ${toString(n.right)}, next = ${toString(n.next)})"

  def node(value: Int, left: Node | Null = null, right: Node | Null = null) =
    val n = Node(value)
    n.left = left
    n.right = right
    n


// idea: 
// - Use BFS to traverse each level
// - on each level, reverse and traverse keeping the prev element
def connect(root: Node): Node = {
  type Level = Int
  val queue: Queue[(Node | Null, Level)] = Queue(root -> 1)
  var prev: Node | Null = null
  var prevLevel = 0
  while (queue.length > 0) {
    val (node, level) = queue.dequeue
    if (node != null) {
      if (node.left != null) {
        node.left.nn.next = node.right
        if (prev != null && prevLevel == level) {
          prev.next = node.left
        }
        prev = node.right
        prevLevel = level
        queue.enqueue(node.left -> (level + 1), node.right -> (level + 1))
      }
    }
  }
  root
}


def traverseBFS(root: Node, f: Node => Unit): Unit = {
  val queue: Queue[Node | Null] = Queue(root)
  while (queue.length > 0) {
    val node = queue.dequeue
    if (node != null) {
      f(node)
      queue.enqueue(node.left, node.right)
    }
  }
}


@main def mainPNR =
  import Node.node
  val n1 = node(1, node(2, node(4), node(5)), node(3, node(6), node(7)))
  connect(n1)
  println("----------------------")
  traverseBFS(n1, node => println((node.value, node.next)))

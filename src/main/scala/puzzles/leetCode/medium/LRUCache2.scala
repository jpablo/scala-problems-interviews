package puzzles.leetCode.medium
package lruCache2


import scala.language.unsafeNulls

import collection.mutable.Map

class Node(val key: Int, val value: Int) {
  var prev = Node.empty
  var next = Node.empty
  override def toString(): String = s"""Node($key, $value)"""
}

object Node {
  val empty = new Node(0, 0)
}

class LRUCache(_capacity: Int) {
  
  val cache = Map.empty[Int, Node]

  // pseudo-nodes, we're not ever suppose to use these values.
  val head = Node.empty
  val tail = Node.empty

  head.next = tail
  tail.prev = head

  // insert before the tail
  def insert(n: Node) = {
    tail.prev.next = n
    n.next = tail
    n.prev = tail.prev
    tail.prev = n
  }

  def remove(n: Node) = {
    n.prev.next = n.next
    n.next.prev = n.prev
  }

  def get(key: Int): Int =
    cache.get(key) match {
      case None => -1
      case Some(n) =>
        remove(n)
        insert(n)
        n.value
    }

  def put(key: Int, value: Int) = {
    cache.get(key).foreach(remove)
    val n = new Node(key, value)
    cache(key) = n
    insert(n)
    if (cache.size > _capacity) {
      val lru = head.next
      remove(lru)
      cache.remove(lru.key)
    }
  }
}


@main def main146_2 =
  val cache = new LRUCache(2)
  println(cache.cache)
  // cache.put(1, 1)
  // println(cache.cache)
  // cache.put(2, 2)
  // println(cache.cache)
  // println(cache.get(1))
  // cache.put(3, 3)
  // println(cache.cache)
  // println(cache.get(2))
  // cache.put(4, 4)
  // println(cache.cache)
  // println(cache.get(1))
  // println(cache.get(3))
  // println(cache.get(4))

  cache.put(2, 1)
  println(cache.cache)
  cache.put(1, 1)
  println(cache.cache)
  cache.put(2, 3)
  println(cache.cache)
  cache.put(4, 1)
  println(cache.cache)
  println("-----------")
  println(cache.get(1))
  println(cache.cache)
  println(cache.get(2))
  println(cache.cache)  
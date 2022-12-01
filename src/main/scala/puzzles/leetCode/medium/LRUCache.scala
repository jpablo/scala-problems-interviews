package puzzles.leetCode.medium


// 146.
// https://leetcode.com/problems/lru-cache/


import collection.mutable.LinkedHashMap

class LRUCache(_capacity: Int) {
  type Key = Int
  type Value = Int
  val cache = LinkedHashMap.empty[Key, Value]

  def get(key: Int): Int =
    cache.remove(key) match {
      case None => -1
      case Some(value) =>
        // move (key,value) to the end
        cache(key) = value
        value
    }

  def put(key: Int, value: Int) = {
    if (cache.contains(key))
      cache.remove(key)
    cache(key) = value
    if (cache.size > _capacity)
      cache.remove(cache.keys.head)
  }

}

@main def main146 =
  val cache = LRUCache(2)
  println(cache.cache)
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
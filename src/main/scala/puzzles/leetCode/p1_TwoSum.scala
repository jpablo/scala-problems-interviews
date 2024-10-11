package puzzles.leetCode

import collection.{View, mutable}
import scala.annotation.tailrec

// 1
// https://leetcode.com/problems/two-sum/


// Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
// You may assume that each input would have exactly one solution, and you may not use the same element twice.
// You can return the answer in any order.

// Hints
// - Properties of the solution
// - degrees of freedom
// - Parameterize

object Solution1 extends App {

  type Index = Int

  // A solution (a, b) satisfies:
  //    a + b = target
  // Parameterizing:
  //    b = target - a

  // for a given `a`, `b = target - a` might or might not be in `nums`
  // if it is in `nums`, we have a solution

  // Array: Index => Int

  def twoSum0(nums: Array[Int], target: Int): Array[Index] = {
    // there might be duplicate numbers, so we need to keep `pairs` around
    // Space: O(n)
    // create two extra copies of the data:
    val pairs: Array[(Int, Index)] = nums.zipWithIndex
    val indices: Map[Int, Index] = pairs.toMap
    // By using `.view` only the first solution is actually calculated
    val solutions: View[Array[Index]] =
      for {
        (a, i) <- pairs.view
        // is the "inverse" in `nums`?
        j <- indices.get(target - a) if i != j
      } yield Array(i, j)
    solutions.head
  }


  // Single pass.
  // The `indices` Map is updated on each element.
  // Early return.
  def twoSum1(nums: Array[Int], target: Int): Array[Index] =
    //    val visited = mutable.Map.empty[Int, Index]
    @tailrec
    def loop(i: Index, visited: Map[Int, Index]): Array[Index] =
      val value = nums(i)
      // is the complement present in nums?
      val complement = target - value
      visited.get(complement) match
        case Some(j) => Array(j, i) // found it, stop
        case None => loop(i + 1, visited + (value -> i))

    loop(0, Map.empty)


  // Single pass, early return (only the first solution is calculated)
  def twoSum(nums: Array[Int], target: Int) = {
    // observed values with their indices
    val visited = mutable.Map.empty[Int, Index]
    nums.view.zipWithIndex.map:
      case (n, i) =>
        println(s"i = $i, visited = $visited")
        visited.get(target - n) match // 19 (no), 2 (no), -10 (yes), -6 (no)
          case None =>
            visited += (n -> i) // (-10 -> 0), (7 -> 1), (19 -> 2), (15 -> 3)
            Array.empty[Index]
          case Some(j) =>
            Array(j, i)
//    .filter(_.nonEmpty)
//    .head
  }


  val examples = List(
    (Array(-10, 7, 19, 15), 9) -> Array(0, 2),
    (Array(3, 3), 6) -> Array(0, 1),
    (Array(2, 7, 11, 15), 9) -> Array(0, 1),
    (Array(3, 2, 4), 6) -> Array(1, 2),
  )

  import upickle.default.writeJs

  for ((input, target), expected) <- examples do
    //    val result = twoSum(input, target)
    val result = twoSum1(input, target)
//    println("--------")
//    println(writeJs(result))
//    println(result.find(_.nonEmpty).toList.flatMap(_.toList))
//    assert(result.find(_.nonEmpty).toList.flatMap(_.toList) == expected.toList, s"${result.toList} != ${expected.toList}")
    assert(result.toList == expected.toList, s"${result.toList} != ${expected.toList}")

  println("Ok")
}

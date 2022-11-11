package puzzles.leetCode

import collection.mutable

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

  // for a given `a`, `target - a` might or might not be in `nums`

  def twoSum0(nums: Array[Int], target: Int): Array[Index] = {
    // there might be duplicate numbers, so we need to keep `pairs` around
    // Space: O(n)
    // create two extra copies of the data:
    val pairs = nums.zipWithIndex
    val indices = pairs.toMap
    // By using `.view` only the first solution is actualy calculated
    val solutions = 
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
  def twoSum1(nums: Array[Int], target: Int): Array[Index] = {
    val indices = mutable.Map.empty[Int, Index]
    def loop(i: Index): Array[Index] =
      // is the complement present in nums?
      indices.get(target - nums(i)) match
        case Some(j) => Array(j, i)
        case None => 
          indices(nums(i)) = i
          loop(i + 1)
    loop(0)
  }


  // Single pass, early return (only the first solution is calculated)
  def twoSum(nums: Array[Int], target: Int): Array[Index] = {
    val indices = mutable.Map.empty[Int, Index]
    val empty = Array.empty[Index]
    nums.view.zipWithIndex.map { case (a, i) => 
      indices.get(target - a) match
        case None => 
          indices(a) = i
          empty
        case Some(j) => 
          Array(j, i)
    }.filter(_.nonEmpty)
    .head
  }


  val examples = List(
    (Array(-10,7,19,15), 9) -> Array(0, 2),
    (Array(3, 3), 6)        -> Array(0, 1),
    (Array(2,7,11,15), 9)   -> Array(0, 1),
    (Array(3, 2, 4), 6)     -> Array(1, 2),
  )

  for ((input, target), expected) <- examples do
    val result = twoSum1(input, target)
    println("--------")
    assert(result.toList == expected.toList, s"${result.toList} != ${expected.toList}")

  println("Ok")
}

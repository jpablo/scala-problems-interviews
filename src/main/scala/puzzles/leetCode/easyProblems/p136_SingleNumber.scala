package puzzles.leetCode


// https://leetcode.com/problems/single-number/

// Given a non-empty array of integers nums, every element appears twice except for one. Find that single one.
// You must implement a solution with a linear runtime complexity and use only constant extra space.


// Constraints:
// 1 <= nums.length <= 3 * 10^4
// -3 * 10^4 <= nums[i] <= 3 * 10^4
//   -30,000 <= nums[i] <= 30,000
// Each element in the array appears twice except for one element which appears only once.



// Idea: 
// - treat input `nums` as indices
// - flip bits twice to detect odd occurrences

def singleNumber(nums: Array[Int]): Int = {
  val idx = Array.ofDim[Boolean](60000)
  // ensure that no negative indices occur
  val min = 30000
  // flip bits on idx at position n
  // if n appears twice the result will flip back to `false`, otherwise it will stay `true`
  for (n <- nums) idx(n + min) = !idx(n + min)
  // find the first `true`
  nums.find(n => idx(n + min)).get
}

// toggle twice each bit that except the odd number 
def singleNumber2(nums: Array[Int]): Int = 
  nums.foldLeft(0)(_^_)

def singleNumber3(nums: Array[Int]) = 
  nums.scanLeft(0)(_^_).map(_.toBinaryString).toList

@main def mainSingleNumber =
  println(singleNumber3(Array(2,2,1)))
  println(singleNumber3(Array(4,1,2,1,2)))
  println(singleNumber3(Array(1)))
  println(singleNumber3(Array(-1)))

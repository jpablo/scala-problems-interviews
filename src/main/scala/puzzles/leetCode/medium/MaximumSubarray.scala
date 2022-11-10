package puzzles.leetCode.medium

import scala.annotation.tailrec

/* 
if all numbers are negative:
    max subarray is the single biggest number
if all numbers are positive:
    max subarray is the whole array

subarray cannot start or end with a negative number
example:
[-2,  1,  -3,  4,  -1,  2, 1,  -5,  4]
      1   -2
               4    3   4  6   1   5     

[5, 4, -1, 7,  8]
 5  9   8  15 23


 */

// subarray cannot start or end with 
// @tailrec
def maxSubArray(nums: Array[Int]): Int = {
  if (nums.isEmpty)
    0
  else {
    var maxSum = nums(0)
    var sum = nums(0)
    var i = 1
    while (i < nums.size) {
      sum = math.max(nums(i), sum + nums(i))
      maxSum = math.max(sum, maxSum)
      i += 1
    }
    maxSum
  }
}


@main def mainMaxSubarray =
  println(maxSubArray(Array(-2,1,-3,4,-1,2,1,-5,4))) // 6
  println(maxSubArray(Array(1))) // 1
  println(maxSubArray(Array(-1))) // -1
  println(maxSubArray(Array(5,4,-1,7,8))) // 23
  println(maxSubArray(Array(-2, 1))) // 1
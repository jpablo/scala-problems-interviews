package puzzles.leetCode


// 88
// https://leetcode.com/problems/merge-sorted-array/

/*
You are given two integer arrays nums1 and nums2, sorted in non-decreasing order, and two integers m and n, representing the number of elements in nums1 and nums2 respectively.

Merge nums1 and nums2 into a single array sorted in non-decreasing order.

The final sorted array should not be returned by the function, but instead be stored inside the array nums1.
To accommodate this, nums1 has a length of m + n, where the first m elements denote the elements that should be merged, and the last n elements are set to 0 and should be ignored. nums2 has a length of n.


*/

// Ideas:
// - start from the end
// - three pointers: 2 read, 1 write

def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
  var r1 = m - 1
  var r2 = n - 1
  var w = m + n - 1
  while (w >= 0 && r2 >= 0) {
    // copy the bigger number to the back of nums1 (at w)
    if (r1 >= 0 && nums1(r1) >= nums2(r2)) {
      nums1(w) = nums1(r1)
      r1 -= 1
    } else {
      nums1(w) = nums2(r2)
      r2 -= 1
    }
    w -= 1
  }
}



@main def main88 =
//  val n1 = Array(1, 2, 3, 0, 0, 0)
//  merge(n1, 3, Array(2, 5, 6), 3)
//  assert(n1 sameElements Array(1, 2, 2, 3, 5, 6))

  val n2 = Array(0)
  merge(n2, 0, Array(1), 1)
  println(n2.toList)
  println("Ok")

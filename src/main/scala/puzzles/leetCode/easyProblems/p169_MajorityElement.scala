package puzzles.leetCode


// https://leetcode.com/problems/majority-element/

// Given an array nums of size n, return the majority element.
// The majority element is the element that appears more than ⌊n / 2⌋ times. 
// You may assume that the majority element always exists in the array.


// Time: O(n log n)
// Space: O(n)
def majorityElement0(nums: Array[Int]): Int = {
  val sorted = nums.sorted
  sorted(sorted.length / 2)        
}

// recursive solution using binary search

// ...


/* Boyer-Moore algorithm */
def majorityElement(nums: Array[Int]): Int = {
  var count = 0
  var candidate: Option[Int] = None
  for (num <- nums) {
    if (count == 0)
      candidate = Some(num)
    count += (if (Some(num) == candidate) 1 else -1)
  }
  candidate.get
}
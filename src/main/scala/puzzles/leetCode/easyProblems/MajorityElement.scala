package puzzles.leetCode


// https://leetcode.com/problems/majority-element/

def majorityElement0(nums: Array[Int]): Int = {
  val sorted = nums.sorted
  sorted(sorted.length / 2)        
}


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
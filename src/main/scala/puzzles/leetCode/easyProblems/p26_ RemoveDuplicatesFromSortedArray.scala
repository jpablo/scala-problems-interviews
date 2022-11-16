package puzzles.leetCode.easyProblems

// Idea:
// - Two indices: read / write
// - transfer data from read to write
def removeDuplicates(nums: Array[Int]): Int = {
  // nums: non-decreasing
  var w = 0
  var r = 1
  while (r < nums.length) {
    if (nums(r) != nums(w)) {
      w += 1
      nums(w) = nums(r)
    }
    r += 1
  }
  w + 1  
}



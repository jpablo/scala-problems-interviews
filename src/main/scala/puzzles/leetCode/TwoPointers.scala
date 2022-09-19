package puzzles.leetCode


def removeDuplicates(nums: Array[Int]): Int = {
  var r = 1
  var w = 0
  while (r < nums.length) {
    if (nums(r) != nums(w)) {
      w += 1
      nums(w) = nums(r)
    }
    r += 1
  }
  w + 1  
}

//.   r
// [1,1,1,2,2,3,3,4]
//. w

// [1,2,1,2,2,3,3,4]
//.   w.  r 

// [1,2,1,2,2,3,3,4]
//.   w.      r 
// [1,2,3,2,2,3,3,4]
//.     w       r 
// [1,2,3,2,2,3,3,4]
//.    .w         r 
// [1,2,3,4,2,3,3,4]
//.    .  w       r


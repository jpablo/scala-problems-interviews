package puzzles.leetCode


// trick: start from the end
def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
  // m = 0
  // n = 1
  var r1 = m - 1 // -1
  var r2 = n - 1 // 0
  var w  = m + n - 1 // 0
  
  while (w >= 0 && r2 >= 0) { 
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



// [0], 0
// [1], 1


//           m       (m + n)
// [1, 2, 3, 0, 0, 0]  |
//        r1       w

//        r2
// [2, 5, 6] |
//           n

//           m       (m + n)
// [1, 2, 2, 3, 5, 6]  |
//  r1
//  w

//  r2
//  | [2, 5, 6] |
//              n


package puzzles.leetCode.medium

// // O(n log n)
// def longestConsecutive(nums: Array[Int]): Int = {
//   // assume: nums is sorted
//   // assert(nums.sorted == nums)
//   if (nums.isEmpty)
//     0
//   else {
//     val (t, c) =
//       nums.indices.drop(1).foldLeft((1, 1)) { case ((total, current), i) =>
//         if (nums(i) - nums(i - 1) == 1)
//           (total, current + 1)
//         else
//           (math.max(total, current), 1)
//       }
//     math.max(t, c)
//   }
// }

// Hint: A modification of the naive brute force approach works.

def longestConsecutive(nums: Array[Int]): Int = {
  var longest = 0
  val numsSet = nums.toSet
  for { 
    num <- nums
    if !numsSet.contains(num - 1)
  } {
    var n = num
    var streak = 1
    while (numsSet.contains(n + 1)) {
      n += 1
      streak += 1
    }
    longest = math.max(longest, streak)
  }
  longest
}

@main def mainLC =
  // assert(longestConsecutive0(Array(100,4,200,1,3,2).sorted) == 4)
  // assert(longestConsecutive0(Array(0,3,7,2,5,8,4,6,0,1).sorted) == 9)

  assert(longestConsecutive(Array(100,4,200,1,3,2)) == 4)
  // assert(longestConsecutive(Array(0,3,7,2,5,8,4,6,0,1)) == 9)
  println("Ok")

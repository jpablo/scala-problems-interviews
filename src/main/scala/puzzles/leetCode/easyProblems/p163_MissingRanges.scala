package puzzles.leetCode

// https://leetcode.com/problems/missing-ranges/



// You are given an inclusive range [lower, upper] and a sorted unique integer array nums, where all elements are in the inclusive range.
// A number x is considered missing if x is in the range [lower, upper] and x is not in nums.
// Return the smallest sorted list of ranges that cover every missing number exactly. That is, no element of nums is in any of the ranges, and each missing number is in one of the ranges.
// Each range [a,b] in the list should be output as:
// "a->b" if a != b
// "a"    if a == b


/* 
Idea
 - scan array looking for gaps from prev to current element

 */

def formatGap (start: Int, end: Int) = 
  if (start == end) start.toString else s"$start->$end"

  
def findMissingRanges(nums: Array[Int], lower: Int, upper: Int): List[String] = {
  val innerGaps = 
    if (nums.isEmpty) 
      IndexedSeq.empty
    else
      for {
        i <- nums.indices.tail
        prev = nums(i - 1)
        cur  = nums(i)
        if prev + 1 != cur
      } yield
        (prev + 1, cur - 1)

  // TODO: add gaps for lower, upper
  innerGaps.map(formatGap.tupled).toList
}

/* Two pointers, cur and prev */
def findMissingRanges0(nums: Array[Int], lower: Int, upper: Int): List[String] = {


  var gaps = List.empty[(Int, Int)]
  var prev = lower - 1

  for (i <- 0 to nums.length) {
    val curr = 
      if (i < nums.length) nums(i) else upper + 1
    println((prev, curr))
    if (prev + 1 <= curr - 1)
      gaps = ((prev + 1) -> (curr - 1)) :: gaps
    prev = curr
  }

  gaps.reverse.map(formatGap.tupled)

}

@main def mainMissingRanges =
  // println ( findMissingRanges(Array(0,1,3,50,75), 0, 99) )     // 0, [0, ..., 75], 99   -> List(2, 4->49, 51->74, 76->99)
  // println ( findMissingRanges(Array(-1), -1, -1) )          //-1, [-1], -1           -> []
  println ( findMissingRanges(Array(), 1, 1) )              // 1, [], 1              -> [1]
  // println ( findMissingRanges(Array(-1), -2, -1) )          // -2, [-1], -1          -> []
  // println ( findMissingRanges(Array(-1000000000,-9999,0,1,2,10,100,1000,999999999,1000000000), -1000000000, 1000000000) )
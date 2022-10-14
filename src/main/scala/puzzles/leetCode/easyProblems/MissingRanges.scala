package puzzles.leetCode


/* Two pointers, cur and prev */

def findMissingRanges(nums: Array[Int], lower: Int, upper: Int): List[String] = {

  def formatGap (start: Int, end: Int) = 
    if (start == end) start.toString else s"$start->$end"

  var gaps = List.empty[(Int, Int)]

  var i = 0
  var prev = lower - 1

  while (i < nums.length + 1) {
    var curr = if (i < nums.length) nums(i) else upper + 1
    if (prev + 1 <= curr - 1)
      gaps = ((prev + 1) -> (curr - 1)) :: gaps
    i += 1
    prev = curr
  }

  gaps.reverse.map(t => formatGap(t._1, t._2))

}

@main def mainMissingRanges =
  println ( findMissingRanges(Array(0,1,3,50,75), 0, 99) ) // 0, [0, ..., 75], 99   -> [(0, 0), (75, 99)]
  println ( findMissingRanges(Array(1,3,50,75), 0, 99) )    // 0, [1, ..., 75], 99   -> [(0, 1), (75, 99)]
  println ( findMissingRanges(Array(-1), -1, -1) )          //-1, [-1], -1           -> [(-1, -1), (-1, -1)]
  println ( findMissingRanges(Array(), 1, 1) )              // 1, [], 1              -> []
  println ( findMissingRanges(Array(-1), -2, -1) )          // -2, [-1], -1          -> [(-2, -1), (-1, -1)]
  println ( findMissingRanges(Array(-1000000000,-9999,0,1,2,10,100,1000,999999999,1000000000), -1000000000, 1000000000) )
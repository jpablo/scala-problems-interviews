package puzzles.leetCode.easyProblems

import scala.annotation.tailrec



/* 
Idea:
  - sliding window (two pointers)
  - Set of chars in current sliding window
 */

type Interval = (Int, Int)

def lengthOfLongestSubstring(s: String): Int = {
  
  def len(i: Interval) = i._2 - i._1
  def max(a: Interval, b: Interval) = if (len(a) >= len(b)) a else b
  
  @tailrec
  def loop(interval: Interval, chars: Set[Char], longest: Interval): Interval = {
    val (l, r) = interval
    if (r >= s.length)
      longest
    else {
      val (interval2, chars2) = 
        if (chars contains s(r))
          ((l + 1, r), chars - s(l))
        else 
          ((l, r + 1), chars + s(r))
      loop(interval2, chars2, max(longest, interval2))
    }
    
  }
  
  len(loop((0, 0), Set.empty, (0, 0)))
}

@main def main3 =
  println(lengthOfLongestSubstring("abcabcbb"))
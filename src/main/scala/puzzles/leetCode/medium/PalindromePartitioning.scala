package puzzles.leetCode.medium

import collection.mutable.ArrayBuffer
import scala.annotation.tailrec

// def allSubstrings(s: String) =
//   for {
//     start <- 0 to s.length
//     end <- (start + 1) to s.length
//   } yield 
//     (start, end-1, s.substring(start, end).nn)

// def allPartitions(s: String): List[List[String]] =
//   (1 to s.length).map(s.grouped).toList.map(_.toList)

def isPalindrome0(s: String, l: Int, h: Int): Boolean = 
  s == s.reverse

@tailrec
def isPalindrome(s: String, l: Int, h: Int): Boolean =
  if (l < h)
    if (s.charAt(l) != s.charAt(h)) false else isPalindrome(s, l + 1, h - 1)
  else
    true

def partition(s: String): List[List[String]] = {
  val result: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer.empty
  dfs(s, 0, result, ArrayBuffer.empty)
  result.toList.map(_.toList)
}


def dfs(s: String, start: Int, result: ArrayBuffer[ArrayBuffer[String]], current: ArrayBuffer[String]): Unit = {
  if (start >= s.length)
    result += current.clone

  for (end <- start until s.length) {
    val ss = s.substring(start, end + 1).nn
    if (isPalindrome(s, start, end)) {
      current += ss
      dfs(s, end + 1, result, current)
      current.remove(current.length - 1)
    }
  }
}


@main def mainPP =
  // println(allPartitions("ab"))
  // println(allPartitions("aab"))
  // println(partition("aab"))
  // println(allSubstrings("aab"))
  // println(allSubstrings("aab").map(_._3).filter(isPalindrome))
  println(partition("aab"))

package puzzles.leetCode.easyProblems

import scala.annotation.tailrec

// 14
// https://leetcode.com/problems/longest-common-prefix/


// Write a function to find the longest common prefix string amongst an array of strings.
//
// If there is no common prefix, return an empty string "".


type Pos = Int

// Idea:
// - Compare each char of the *first* word against all others

def longestCommonPrefix(words: Array[String]): String = {

  val firstWord = words(0)

  @tailrec
  def loop(i: Pos = 0, acc: String = ""): String = {
    if (i < firstWord.length && commonAtPosition(firstWord(i), i, 0))
      // if the char at pos `i` (in head) is the same in all words,
      // add it to the running prefix
      loop(i + 1, acc + firstWord(i))
    else
      acc
  }

  @tailrec
  def commonAtPosition(c: Char, cPos: Pos, wordPos: Int = 0): Boolean =
    if (wordPos < words.length)
      if (cPos < words(wordPos).length && c == words(wordPos)(cPos))
        // chars are the same for current word, go to next word
        commonAtPosition(c, cPos, wordPos + 1)
      else
        false
    else
      true

  loop(i = 0)
}

@main def main14(): Unit =
  assert(longestCommonPrefix(Array("flower","flow","flight")) == "fl")
  assert(longestCommonPrefix(Array("dog","racecar","car")) == "")
  println("Ok")

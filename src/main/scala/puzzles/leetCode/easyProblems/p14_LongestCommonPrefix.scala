package puzzles.leetCode

// 14
// https://leetcode.com/problems/longest-common-prefix/


// Write a function to find the longest common prefix string amongst an array of strings.
// 
// If there is no common prefix, return an empty string "".


type Pos = Int

// Idea:
// - Compare each char of the *first* word against all others
def longestCommonPrefix(strs: Array[String]): String = {
  
  val head = strs(0)

  def loop(i: Pos = 0, prefix: String = ""): String = {
    if (i < head.length && commonAtPosition(head(i), i))
      // if the char at pos `i` (in head) is the same in all words,
      // add it to the running prefix 
      loop(i + 1, prefix + head(i))
    else
      prefix        
  }
  
  def commonAtPosition(c: Char, cPos: Pos, j: Int = 0): Boolean =
    if (j < strs.length)
      if (cPos < strs(j).length && c == strs(j)(cPos))
        // chars are the same for current word, go to next word
        commonAtPosition(c, cPos, j + 1)
      else 
        false
    else
      true

  loop()
}

@main def main14 =
  assert(longestCommonPrefix(Array("flower","flow","flight")) == "fl")
  assert(longestCommonPrefix(Array("dog","racecar","car")) == "")
  println("Ok")

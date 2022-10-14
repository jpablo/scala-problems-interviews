package puzzles.leetCode



def longestCommonPrefix(strs: Array[String]): String = {
  type Pos = Int
  
  def go(i: Pos = 0, prefix: String = ""): String = {
    if ( i < strs(0).length && commonAtPosition( strs(0)(i), i) )
      go(i + 1, prefix + strs(0)(i))
    else
      prefix        
  }
  
  def commonAtPosition(c: Char, cPos: Pos, w: Int = 0): Boolean =
    if (w < strs.length)
      if (cPos < strs(w).length && c == strs(w)(cPos))
        commonAtPosition(c, cPos, w + 1)
      else false
    else
      true


  go()
}


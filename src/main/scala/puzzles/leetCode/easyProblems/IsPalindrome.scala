package puzzles.leetCode


/**
  * Input string is scanned 3 times
  *
  */
def isPalindrome(s: String): Boolean = {
  val lower = s.toLowerCase.filter(_.isLetterOrDigit)
  lower == lower.reverse
}

/**
  * Two pointers moving in opposite directions
  */
def isPalindrome2(s: String): Boolean = {
  var i = 0
  var j = s.length - 1
  var isPal = true
  while (i < j && isPal)
    if (!s(i).isLetterOrDigit)
      i += 1
    else if (!s(j).isLetterOrDigit)
      j -= 1
    else if (s(i).toLower != s(j).toLower)
      isPal = false
    else {
      i += 1
      j -= 1
    }

  isPal
}

/**
  * Two pointers moving in opposite directions, tail rec
  */
def isPalindrome3(str: String): Boolean =
  def go (s: Int, e: Int): Boolean =
    val c1 = str(s)
    val c2 = str(e)
    if s < e then
      if      !c1.isLetterOrDigit       then go(s + 1, e)
      else if !c2.isLetterOrDigit       then go(s, e - 1)
      else if  c1.toLower != c2.toLower then false
      else                                   go(s + 1, e - 1)
    else
      true
  go(0, str.length - 1)


@main def mainIsPalindrome =
  println(isPalindrome3(""))
  println(isPalindrome3("a"))
  println(isPalindrome3("aba"))
  println(isPalindrome3("abab"))
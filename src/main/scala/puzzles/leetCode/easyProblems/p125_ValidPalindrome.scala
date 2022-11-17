package puzzles.leetCode.easyProblems

// A phrase is a palindrome if, after converting all uppercase letters into lowercase letters and removing all non-alphanumeric characters, 
// it reads the same forward and backward. Alphanumeric characters include letters and numbers.
// Given a string s, return true if it is a palindrome, or false otherwise

/* 
Idea:
  - two symmetric pointers moving from to the center
*/

def isPalindrome(s: String): Boolean = {
  var i = 0
  var j = s.length - 1
  var isPal = true // i.e. an empty string
  while (i < j && isPal)
    val (l, r) = (s(i), s(j))
    if (!l.isLetterOrDigit)
      i += 1
    else if (!r.isLetterOrDigit)
      j -= 1
    else if (l.toLower != r.toLower)
      isPal = false
    else {
      i += 1
      j -= 1
    }

  isPal
}

@main def main125 =
  assert(isPalindrome("A man, a plan, a canal: Panama"))
  assert(!isPalindrome("race a car"))
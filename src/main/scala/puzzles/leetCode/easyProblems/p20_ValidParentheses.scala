package puzzles.leetCode.easyProblems

import scala.annotation.tailrec

// 20
// https://leetcode.com/problems/valid-parentheses/

// Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

// An input string is valid if:

// Open brackets must be closed by the same type of brackets.
// Open brackets must be closed in the correct order.
// Every close bracket has a corresponding open bracket of the same type.

val matches = Map(
  '(' -> ')',
  '{' -> '}',
  '[' -> ']',
)


// Ideas:
// - Use a Stack
// - Use helper functions with significant names (to help reading)
// - Decision tables (via simultaneous pattern matching)
def isValid(input: String): Boolean = {
  
  def isStart(t: Char)           = matches.keySet contains t
  def validPair(p: (Char, Char)) = matches.toSet  contains p

  @tailrec
  def loop(tokens: List[Char], stack: List[Char] = List.empty): Boolean = (tokens, stack) match {
    case (Nil, Nil)         => true
    case (Nil, _ :: _)      => false
    case (t :: ts, Nil)     => loop(ts, List(t))
    case (t :: ts, s :: ss) =>
      if (isStart(t)) 
        loop(ts, t :: s :: ss)
      else if (validPair(s -> t)) // t is the correct end
        loop(ts, ss)              // continue with next token
      else // t is the incorrect end
        false
    }
  if (input.isEmpty)
    true
  else
    loop(input.toList)
}

@main def main20 =
  assert(isValid("()"))
  assert(isValid("()[]{}"))
  assert(!isValid("(]"))
  println("Ok")
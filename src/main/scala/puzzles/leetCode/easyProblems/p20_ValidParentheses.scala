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
  '[' -> ']'
)

// Ideas:
// - Use a Stack
// - Use helper functions with significant names (to help reading)
// - Decision tables (via simultaneous pattern matching)

// Why a stack: "The most recently opened parenthesis must be closed first"

def isValid(input: String): Boolean = {

  def isStart(t: Char) = matches.keySet contains t

  def isValidPair(p: (Char, Char)) = matches.toSet contains p

  // Notes: tokens and previousTokens are in opposite order!
  @tailrec
  def loop(previousTokens: List[Char], tokens: List[Char]): Boolean =
    (previousTokens, tokens) match
      case (Nil, Nil) => true
      case (_, Nil)   => false

      case (Nil, nextToken :: ts) => loop(nextToken :: Nil, ts) // start

      case (prev :: pt, next :: tt) =>
        // 3 cases:
        if isStart(next) then
          loop(next :: previousTokens, tt)
        // next is not a start
        else if isValidPair(prev -> next) then
          loop(pt, tt) // remove the pair and continue
        else
          false

  if (input.isEmpty)
    true
  else
    loop(Nil, input.toList)
}

@main def main20(): Unit =
  assert(isValid("()"))
  assert(isValid("()[]{}"))
  assert(!isValid("(]"))
  println("Ok")

package puzzles

import scala.annotation.tailrec

// if endif
// begin end
// start stop


// Keys:
  // 0. Understand the order of start/end of tokens (it's similar to indentation)
  // 1. Create a dict of pairs of matches
  // 2. pattern match on both lists at the same time




def balanced(input: String, matches: Map[String, String]): Boolean = {
  
  def isStart(token: String) =
    matches.keySet.contains(token)
  
  def isClosing(token: String) =
    matches.values.toSet.contains(token)
  

  def validPair(start: String, end: String) =
    matches.toSet.contains((start, end))


  @tailrec
  def loop(starts: List[String], remaining: List[String]): Boolean = {
    remaining match {
      case token :: rest =>
        if (isClosing(token)) {
          val start = starts.headOption.getOrElse("#")
          if (!validPair(start, token))
            false
          else
            loop(starts, remaining)
        } else
          loop(token :: starts, rest)
      case Nil =>
        starts.nonEmpty
    }
      
    
  }


  @tailrec
  def go0(stack: List[String], remaining: List[String]): Boolean =
    (stack, remaining) match {
      case (Nil, Nil)    => true

      case (_ :: _, Nil) => false

      case (Nil, token :: tokens) => go0(List(token), tokens)

      case (start :: starts, token :: tokens) =>
        
        if      isStart(token)          then go0(token :: stack, tokens)
        else if validPair(start, token) then go0(starts,         tokens)
        else false
    }

  if input.isEmpty then 
    true
  else
    loop(List.empty, input.split(' ').toList)
  }


val examples = List(
  "" -> true,
  "if endif" -> true,
  "begin end" -> true,
  "start stop" -> true,
  "start stop if endif" -> true,
  "if begin end endif" -> true,
  "start stop if begin end endif" -> true,
  "start stop if begin endif" -> false,
  "start" -> false,
  "stop" -> false,
  "if begin endif stop" -> false,
)


val matches = Map(
  "if" -> "endif",
  "begin" -> "end",
  "start" -> "stop",
)


@main def run =
  for (input, expected) <- examples do
    val result = balanced(input, matches)
    assert(result == expected, s"$input | $result | expected: $expected")
    println("Ok")
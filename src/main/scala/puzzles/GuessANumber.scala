package puzzles


val x = 1

enum Response:
  case Equal
  case Smaller
  case Greater

import Response.*

def query (y: Int) =
  val x = 1618235
  if x == y     then Equal
  else if x < y then Smaller
  else               Greater


def guess (lower: Int, upper: Int): Boolean  =
  val middle = (lower + upper) / 2
  val answer = query(middle)
  println((middle, answer))
  answer match
    case Equal   => true
    case Smaller => guess(lower, middle - 1)
    case Greater => guess(middle + 1, upper)


@main def runGuess =
  println(guess(1, 2097151))
  

package puzzles

import scala.annotation.tailrec

// f 0 = 0
// f 1 = 1
// f 2 = f 1 + f 0 = 1 + 0 = 2
// f 3 = f 2 + f 1 = 2 + 1 = 3
//...
// f n = f (n - 1) + f (n - 2)

def fib(n: Int): Int =
  @tailrec
  def go(curr: Int, prev: Int, i: Int): Int =
    if i > n then
      curr
    else 
      go(
        curr = curr + prev, 
        prev = curr, 
        i    = i + 1
      )
  if n < 2 then n else go(1, 0, 2)


def fib2 (n: Int) : Int =
  var prev = 1
  var cur = 1
  var i = n
  while i > 2 do
    val next = cur + prev
    prev = cur  // (prev is overwritten!)
    cur = next
    i -= 1
  cur


def lastDigit(n: Int): Int = ???

@main def runFib() =
  // println(fib(2))
  for a <- 0 until 10 do
    println((a, fib(a)))

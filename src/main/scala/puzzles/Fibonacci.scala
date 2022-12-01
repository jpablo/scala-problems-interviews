package puzzles

import scala.annotation.tailrec
import collection.mutable.Map as MMap


// f 0 = 0
// f 1 = 1
// f 2 = f 1 + f 0 = 1 + 0 = 1
// f 3 = f 2 + f 1 = 1 + 1 = 2
// f 4 = f 3 + f 2 = 2 + 1 = 3
//...
// f n = f (n - 1) + f (n - 2)


def fibSpec(f: Int => Int, n: Int) = 
  assert(n >= 1)
  if n == 1 || n == 2 then 
    f(n) == 1
  else
    f(n + 2) == f(n) + f(n + 1)


// naive recursive
def fib0(n: Int): Int =
  if n <= 1 then 
    n
  else
    // List(1, 2).map(m => fib0(n - m)).sum
    fib0(n - 1) + fib0(n - 2)

  // f 7 =
  // f 6 + f 5 =
  // (f 5 + f 4) + f 5 =
  // ((f 4 + f 3) + f 4) + f 5 =
  // (((f 3 + f 2) + f 3) + f 4) + f 5 =
  // ((((f 2 + f 1) + f 2) + f 3) + f 4) + f 5 =
  // (((((f 1 + f 0) + f 1) + f 2) + f 3) + f 4) + f 5 =
  
  // (((((1 + 0) + f 1) + f 2) + f 3) + f 4) + f 5 =
  
  // ((((1 + f 1) + f 2) + f 3) + f 4) + f 5 =
  // ((((1 + 1) + f 2) + f 3) + f 4) + f 5 =
  // (((2 + f 2) + f 3) + f 4) + f 5 =
  // (((2 + 1) + f 3) + f 4) + f 5 =
  // ((3 + f 3) + f 4) + f 5 =
  // ((3 + 2) + f 4) + f 5 =
  // (5 + f 4) + f 5 =
  // (5 + 3) + f 5 =
  // 8 + f 5 =
  // 8 + 5 =
  // 13 =

// DP + cache
def fibRecCache(n: Int, cache: MMap[Int, Long] = MMap.empty): Long =
  if n <= 1 then 
    n
  else if cache.contains(n) then
    cache(n)
  else
    cache(n) = fibRecCache(n - 1, cache) + fibRecCache(n - 2, cache)
    cache(n)


// DP with tabulation
def fibTabulated(n: Int): Int =
  val solutions = Array.ofDim[Int](n + 2) // 0 <= i < n + 2
  if 1 < solutions.length then solutions(1) = 1
  for i <- solutions.indices do
    if i + 1 < solutions.length then solutions(i + 1) += solutions(i)
    if i + 2 < solutions.length then solutions(i + 2) += solutions(i)
    println((i, solutions.toList))
  solutions(n)



// tail rec version
def fibTailRec(n: Int): Int =
  @tailrec def loop(c: Int, p: Int, i: Int): Int =
    if i >= n then
      c
    else 
      loop(c + p, c, i + 1)
  loop(1, 0, 1)
  
  // loop(1, 0, n-0)
  // loop(1, 1, n-1)
  // loop(2, 1, n-2)
  // loop(3, 2, n-3)
  // loop(5, 3, n-4)
  // loop(8, 5, n-5)
  // loop(13, 8, n-6)
  // loop(21, 13, n-7)

  // loop(a    , b, n    ) => 
  // loop(a + b, a, n - 1)


def fibWhile (n: Int) : Int =
  var prev = 1
  var cur = 1
  var i = n
  while i > 2 do
    val next = cur + prev
    prev = cur  // (prev is overwritten!)
    cur = next
    i -= 1
  cur


import puzzles.util.*

@main def runFib() =
  def compare(n: Int) =
    (n, fibTailRec(n).time._2, fibWhile(n).time._2, fibTabulated(n).time._2, fibRecCache(n).time._2)

  println(fibTabulated(5))
  // println((fibTabulated(10), fibTailRec(10)))
  // for a <- 0 until 500 do println(compare(a))

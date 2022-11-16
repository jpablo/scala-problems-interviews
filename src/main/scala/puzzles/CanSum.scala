package puzzles

import scala.reflect.ClassTag


def canSum(targetSum: Int, numbers: List[Int]): Boolean =
  val solutions = Array.ofDim[Boolean](targetSum + 1)
  solutions(0) = true
  for 
    i   <- solutions.indices if solutions(i)
    num <- numbers if i + num < solutions.length
  do
    solutions(i + num) = true
  solutions.last

def howSum(targetSum: Int, numbers: List[Int]): List[Int] | Null =
  val solutions = Array.ofDim[List[Int] | Null](targetSum + 1)
  // seed value
  solutions(0) = List.empty
  for 
    i   <- solutions.indices if solutions(i) != null 
    num <- numbers if i + num < solutions.length
  do
    // unconditionally replace existing value
    solutions(i + num) = num :: solutions(i).nn
  solutions.last


/* 
Ideas:
  - start with a seed: the recursive base case
  - only move from valid positions
 */
def bestSum(targetSum: Int, numbers: List[Int]): List[Int] | Null = 
  val solutions = Array.ofDim[List[Int] | Null](targetSum + 1)
  // seed value:
  solutions(0) = List.empty
  for 
    i   <- solutions.indices if solutions(i) != null
    num <- numbers
    j    = i + num if j < solutions.length
  do
    val candidate = num :: solutions(i).nn
    if solutions(j) == null || solutions(j).nn.length > candidate.length then
      solutions(j) = candidate
  solutions.last


// ------------------------------------------
  
def genSum[A: ClassTag, B <: A](targetSum: Int, numbers: List[Int], init: A, empty: B)(combine: (Int, A, A) => A): A =
  val solutions = Array.fill[A](targetSum + 1)(empty)
  solutions(0) = init // base case
  for 
    i   <- solutions.indices if solutions(i) != empty // only valid positions
    num <- numbers           if i + num < solutions.length
  do
    solutions(i + num) = combine(num, solutions(i), solutions(i + num))
  solutions.last


def canSum2(targetSum: Int, numbers: List[Int]): Boolean =
  genSum(targetSum, numbers, true, false)((_, _, _) => true)

def howSum2(targetSum: Int, numbers: List[Int]): List[Int] | Null =
  genSum(targetSum, numbers, List.empty, null)((num, current, _) => num :: current.nn)

def bestSum2(targetSum: Int, numbers: List[Int]): List[Int] | Null =   
  genSum(targetSum, numbers, List.empty, null) { (num, current, target) => 
    val candidate = num :: current.nn
    if target == null || target.nn.length > candidate.length then
      candidate
    else
      target
  }

@main def mainCS =
  assert(canSum2(7, List(2, 3))       == true)
  assert(canSum2(7, List(5, 3, 4, 7)) == true)
  assert(canSum2(7, List(2, 4))       == false)
  assert(canSum2(8, List(2, 3, 5))    == true)
  assert(canSum2(300, List(7, 14))    == false)
  println("Ok: canSum")
  
  assert(howSum2(7, List(2, 3))       == List(2, 2, 3))
  assert(howSum2(7, List(5, 3, 4, 7)) == List(3, 4))
  assert(howSum2(7, List(2, 4))       == null)
  assert(howSum2(8, List(2, 3, 5))    == List(2,2,2,2))
  assert(howSum2(300, List(7, 14))    == null)
  println("Ok: howSum")
  
  assert(bestSum2(7, List(5, 3, 4, 7)) == List(7))
  assert(bestSum2(8, List(2, 3, 5))    == List(5, 3))
  assert(bestSum2(8, List(1, 4, 5))    == List(4, 4))
  assert(bestSum2(100, List(1, 2, 5, 25)) == List(25, 25, 25, 25))
  assert(bestSum2(100, List(25, 1, 2, 5)) == List(25, 25, 25, 25))
  println("Ok: bestSum")
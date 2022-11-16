package puzzles

import collection.mutable

// DP, Memoization
def gridTraveler0(m: Int, n: Int, cache: mutable.Map[(Int, Int), Long] = mutable.Map.empty): Long = 
  val key = (m, n)
  if cache.contains(key) then cache(key)
  else if m == 1 && n == 1 then 1
  else if m == 0 || n == 0 then 0
  else
    cache(key) = gridTraveler0(m - 1, n, cache) + gridTraveler0(m, n - 1, cache)
    cache(key)
    

// DP, Tabulation
def gridTraveler(m: Int, n: Int): Long = 
  val table = Array.ofDim[Long](m + 1, n + 1)
  table(1)(1) = 1
  for 
    i <- 0 to m
    j <- 0 to n 
  do
    val current = table(i)(j)
    if j + 1 <= n then table(i)(j + 1) += current
    if i + 1 <= m then table(i + 1)(j) += current
  table(m)(n)

@main def mainGT =
  assert(gridTraveler(1, 1) == 1)
  assert(gridTraveler(2, 3) == 3)
  assert(gridTraveler(3, 2) == 3)
  assert(gridTraveler(3, 3) == 6)
  assert(gridTraveler0(3, 3) == 6)
  assert(gridTraveler(18, 18) == 2333606220L)
  assert(gridTraveler0(18, 18) == 2333606220L)
  println("Ok")
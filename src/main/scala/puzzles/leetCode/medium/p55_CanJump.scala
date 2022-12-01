package puzzles.leetCode.medium



// You are given an integer array nums. You are initially positioned at the array's first index, and each element in the array represents your maximum jump length at that position.

// Return true if you can reach the last index, or false otherwise.



/* 

Idea: 
  - 
 */

def canJump(nums: Array[Int]): Boolean = {
  var goal = nums.indices.last
  for (i <- nums.indices.reverse)
    if (i + nums(i) >= goal)
      goal = i
  goal == 0
    
}

def canJumpRec(nums: Array[Int], position: Int = 0): Boolean =
  println(position)
  if position >= nums.length - 1 then
    true
  else
    var done = false
    var i = 1
    val maxJump = math.min(position + nums(position), nums.length - 1)
    while position + i <= maxJump && !done do
      if canJumpRec(nums, position + i) then
        done = true
      i += 1
    done

// memo: -1 => false, 0 => unknown, 1 => true
def canJumpDP(nums: Array[Int], position: Int = 0)(memo: Array[Int] = Array.ofDim[Int](nums.length)): Boolean =
  memo(memo.length - 1) = 1
  if memo(position) != 0 then
    if memo(position) == 1 then true else false
  else
    var done = false
    var i = 1
    val maxJump = math.min(position + nums(position), nums.length - 1)
    while position + i <= maxJump && !done do
      if canJumpDP(nums, position + i)(memo) then
        memo(position) = 1
        done = true
      i += 1
    if !done then
      memo(position) = -1
    println((position, memo.toList))
    done



@main def main56 =
  assert(canJumpDP(Array(2,3,1,1,4))())
  println("-----------")
  assert(!canJumpDP(Array(3,2,1,0,4))())
  println("Ok")
package puzzles.leetCode.medium

// - Atras para adelante
// - Reducir a un subproblema
def canJump(nums: Array[Int]): Boolean = {
  var goal = nums.indices.last
  for (i <- nums.indices.reverse)
    if (i + nums(i) >= goal)
      goal = i
  goal == 0
}

@main def mainCanJump =
  println(canJump(Array(2,3,1,1,4))) // true
  println(canJump(Array(3,2,1,0,4))) // false
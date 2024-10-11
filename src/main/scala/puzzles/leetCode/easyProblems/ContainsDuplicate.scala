package puzzles.leetCode.easyProblems

def containsDuplicate(nums: Array[Int]): Boolean =
  nums.groupBy(identity).exists { case (_, is) => is.length > 1 }


@main def mainDuplicate(): Unit =
  println(containsDuplicate(Array(1,2,3,1)))
  println(containsDuplicate(Array(1,2,3,4)))


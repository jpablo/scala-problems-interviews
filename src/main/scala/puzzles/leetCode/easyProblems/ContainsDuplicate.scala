package puzzles.leetCode


def containsDuplicate(nums: Array[Int]): Boolean =
  nums.groupBy(identity).find { case (_, is) => is.length > 1}.nonEmpty


@main def mainDuplicate =
  println(containsDuplicate(Array(1,2,3,1)))
  println(containsDuplicate(Array(1,2,3,4)))


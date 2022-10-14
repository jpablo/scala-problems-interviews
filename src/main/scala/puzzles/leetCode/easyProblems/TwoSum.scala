package puzzles.leetCode


object Solution1 extends App {

  type Index = Int
  
  
  def twoSum0(nums: Array[Int], target: Int): Array[Int] = {
    val numsWithIndex = nums.zipWithIndex
    val indices = numsWithIndex.toMap
    val ret = 
      for {
        xi <- numsWithIndex
        (x, i) = xi
        j <- indices.get(target - x)
        if i != j
      }
      yield Array(i, j)
    ret.head
  }

  
  def twoSum1(nums: Array[Int], target: Int): Array[Int] = {
    val indices = collection.mutable.Map.empty[Int, Index]
    nums.indices.foreach(i => indices += (nums(i) -> i))

    def go(i: Index): Array[Index] = {
      // if the solution doesn't exist this will throw an exception
      val complement = target - nums(i)
      indices.get(complement) match {
        case Some(j) if i != j => Array(i, j)
        case _ => go(i + 1)
      }
    }
    go(0)
  }

  
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val indices = collection.mutable.Map.empty[Int, Index]

    def go(i: Index): Array[Index] = {
      val complement = target - nums(i)
      if (indices contains complement)
        val j = indices(complement)
        Array(indices(complement), i)
      else {
        indices += (nums(i) -> i)
        go(i + 1)
      }
    }

    go(0)
  }

  



  val examples = List(
    (Array(-10,7,19,15), 9) -> Array(0, 2),
    (Array(3, 3), 6) -> Array(0, 1),
    (Array(2,7,11,15), 9) -> Array(0, 1),
    (Array(3, 2, 4), 6) -> Array(1, 2)
  )

  for ((input, target), expected) <- examples do
    val result = twoSum(input, target)
    assert(result.toList == expected.toList, s"${result.toList} != ${expected.toList}")

  println("Ok")
}

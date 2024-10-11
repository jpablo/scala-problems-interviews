package puzzles.leetCode.easyProblems

import scala.annotation.tailrec

object p35_SearchInsertPosition extends App {

  type Pos = Int

  def searchInsert0(nums: Array[Int], target: Int): Int = {
    var a = 0
    var b = nums.length - 1
    while a <= b do
      val mid = a + (b - a) / 2
      if nums(mid) == target then
        return mid
      if nums(mid) < target then
        a = mid + 1
      else
        b = mid - 1
    a
  }

  def searchInsert(nums: Array[Int], target: Int): Int =
    @tailrec
    def loop(a: Pos, b: Pos): Pos =
      if a > b then a
      else
        val mid = a + (b - a) / 2
        nums(mid).compare(target) match
          case 0  => mid
          case -1 => loop(mid + 1, b)
          case 1  => loop(a, mid - 1)
    loop(0, nums.length - 1)


  assert(searchInsert(Array(1, 3, 5, 6), 5) == 2)
  assert(searchInsert(Array(1, 3, 5, 6), 2) == 1)
  assert(searchInsert(Array(1, 3, 5, 6), 7) == 4)

}

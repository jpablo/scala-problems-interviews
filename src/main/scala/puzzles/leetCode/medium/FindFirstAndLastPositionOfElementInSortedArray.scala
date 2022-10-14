package puzzles.leetCode.medium

import scala.annotation.tailrec

object SolutionSearchRange extends App {

  type Index = Int
  type Interval = (Index, Index)

  // nums is sorted: ∀ (i j: nums.indices), i <= j ⟹ nums(i) <= nums(j)
  def binSearch(nums: Array[Int], target: Int)(a: Index = 0, b: Index = nums.length - 1): Index = {
    if (b < a)
      -1
    else
      val midI: Index = a + (b - a) / 2
      val midV = nums(midI)
      if      (midV == target) midI
      else if (midV <  target) binSearch(nums, target)(midI + 1, b)
      else                  binSearch(nums, target)(a, midI - 1)
  }

   // nums is sorted: ∀ (i j: nums.indices), i <= j ⟹ nums(i) <= nums(j)

  // side: true -> left, false -> right
  def binSearchBorder0(nums: Array[Int], target: Int, side: Boolean = true)(a: Index = 0, b: Index = nums.length - 1): Index = {
    val next = binSearchBorder0(nums, target, side)
    if (b < a)
      -1
    else
      val midI: Index = a + (b - a) / 2
      val midV = nums(midI)
      val midVm1 = if (midI - 1 > 0) nums(midI - 1) else target - 1
      val midVp1 = if (midI + 1 < nums.length) nums(midI + 1) else target + 1

      if (side) {
        if (midV == target) 
          if (midVm1 < target) midI 
          else next(a, midI - 1)

        else if (midV < target) next(midI + 1, b) 
        else next(a, midI - 1)

      } else {
        if (midV == target) 
          if (midVp1 > target) midI 
          else next(midI + 1, b)

        else if (midV < target) next(midI + 1, b) 
        else next(a, midI - 1)
      }
  }

  def binSearchBorder1(nums: Array[Int], target: Int, side: Boolean = true)(a: Index = 0, b: Index = nums.length - 1): Index = {
    val next = binSearchBorder1(nums, target, side)
    if (b < a)
      -1
    else
      val midI: Index = a + (b - a) / 2
      val midV = nums(midI)

      val leftV  = if (midI - 1 > 0) nums(midI - 1) else target - 1
      val rightV = if (midI + 1 < nums.length) nums(midI + 1) else target + 1

      // println(s"[side: $side] target: $target, (a, b): ($a, $b), midI: $midI, (leftV: $leftV, midV: $midV, rightV: $rightV)")

      if      ( side && midV == target &&   leftV < target)  midI
      else if ( side && midV == target && !(leftV < target)) next(a, midI - 1)
      else if ( side &&   midV < target ) next(midI + 1, b)
      else if ( side &&   midV > target ) next(a, midI - 1)

      else if (!side && midV == target &&   rightV > target)  midI
      else if (!side && midV == target && !(rightV > target)) next(midI + 1, b)
      else if (!side &&   midV < target ) next(midI + 1, b)
      else if (!side &&   midV > target ) next(a, midI - 1)
      else
        -1
  }

  @tailrec
  def binSearchBorder(nums: Array[Int], target: Int, side: Boolean = true)(a: Index = 0, b: Index = nums.length - 1): Index = {
    inline def next = binSearchBorder(nums, target, side)
    if (b < a)
      -1
    else {
      val midI: Index = a + (b - a) / 2
      val midV = nums(midI)
      val leftV  = if (midI - 1 >= 0)          nums(midI - 1) else target - 1
      val rightV = if (midI + 1 < nums.length) nums(midI + 1) else target + 1

      // println(s"[side: $side] target: $target, (a, b): ($a, $b), midI: $midI, (leftV: $leftV, midV: $midV, rightV: $rightV)")

      if     ((midV == target) && ((side && leftV < target) || (!side && rightV > target))) midI
      else if ((midV > target) || ( side && midV == target && !( leftV < target))) next(a, midI - 1)
      else if ((midV < target) || (!side && midV == target && !(rightV > target))) next(midI + 1, b)
      else
        -1
    }
  }


  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    val l = binSearchBorder(nums, target, true)()
    val r = binSearchBorder(nums, target, false)()
    Array(l, r)
  }

 
  // println(searchRange(Array(5,7,7,8,8,10), 5).toList)   // (0, 0)
  // println(searchRange(Array(5,7,7,8,8,10), 7).toList)   // (1, 2)
  // println(searchRange(Array(5,7,7,8,8,10), 8).toList)   // (3, 4)
  // println(searchRange(Array(5,7,7,8,8,10), 10).toList)  // (5, 5)
  // println(searchRange(Array(5,7,7,8,8,10), 11).toList)  // (-1, -1)
  // println(searchRange(Array(2, 2), 2).toList) // (0, 1)
  println(searchRange(Array(1,1,2), 1).toList) // (0, 1)


}
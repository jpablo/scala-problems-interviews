package puzzles.leetCode.medium

// https://leetcode.com/problems/subsets/

trait Show[A]:
  def show(a: A): String

def show[A](a: A)(using inst: Show[A]) = inst.show(a)
given Show[Int] with { def show(a: Int) = a.toString }
given [A](using inst: Show[A]):Show[List[A]] with { def show(lst: List[A]) = lst.map(inst.show).mkString("[", ", ", "]") }


def subsets(nums: Array[Int]): List[List[Int]] = {
  var output = List(List.empty[Int])
  for (n <- nums) {
    val next = output.map(n :: _)
    println((n, show(output), show(next)))
    output ++= next
  }
  output
}


@main def mainSubsets =
  // println(subsets(Array()))
  // println(subsets(Array(1)))
  // println(subsets2(Array(1, 2)))
  println(show(subsets(Array(1, 2, 3))))

package trees.binarySearch


// https://stackoverflow.com/questions/21586085/difference-between-binary-search-and-binary-search-tree


enum BSTree[+A]:
  case Empty
  case Node(elem: A, left: BSTree[A], right: BSTree[A])

object BSTree:

  case class Inclusive(min: Long, max: Long):
    def contains(elem: Long) =
      min <= elem && elem <= max

  def node[A](a: A, left: BSTree[A] = Empty, right: BSTree[A] = Empty) = 
    Node(a, left, right)


  def fromSortedArray(nums: Array[Int]): BSTree[Int] =
    def go (l: Int, r: Int): BSTree[Int] =
      println((l, r))
      assume(0 <= l && l <= r && r <= nums.length)
      val mid = (l + r) / 2
      if l > r || mid < 0 || mid >= nums.length then 
        Empty
      else
        val left  = go(l, mid - 1)
        val right = go(mid + 1, r)
        Node(nums(mid), left, right)              
    go(0, nums.length)


  extension (tree: BSTree[Int])

    def validate: Boolean = 
      def isValid(t: BSTree[Int], interval: Inclusive) : Boolean = t match
        case Empty => true
        case Node(elem, left, right) =>
          interval.contains(elem) &&
          isValid(left,  Inclusive(interval.min, elem.toLong - 1)) && 
          isValid(right, Inclusive(elem.toLong + 1, interval.max))
      isValid(tree, Inclusive(Int.MinValue, Int.MaxValue))

    def contains(element: Int): Boolean = tree match
      case Empty => false
      case Node(elem, left, right) =>
        if elem == element then 
          true
        else if elem < element then 
          right.contains(element)
        else 
          left.contains(element)


      
@main def mainBS =
  import BSTree.*

  val t1 = node(5, node(1), node(4, node(3), node(6)))
  val t2 = node(5, node(1), node(7, node(6), node(10)))
  val t3 = node(1, node(1))
  val t4 = node(-2147483648, node(-2147483648))

  assert(t1.validate == false)
  assert(t2.validate == true)
  assert(t3.validate == false)
  assert(t4.validate == false)

  println(BSTree.fromSortedArray(Array(0, 1)))
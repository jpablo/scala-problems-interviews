package com.rockthejvm.lists

import com.rockthejvm.lists.RList.from
import scala.util.Random
import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /**
    * Standard functions
    */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
    * Easy problems
    */
  // get element at a given index

  def apply(index: Int): T

  def foldLeft[A](init: A)(step: (A, T) => A): A = {
    @tailrec
    def go(ts: RList[T], acc: A): A = ts match
      case RNil => acc
      case h :: t => go(t, step(acc, h))

    go(this, init)
  }

  // the size of the list
  def length: Int = {
    // go (acc, [])     => acc
    // go (acc, _ :: t) => go(1 + acc, t)
    @tailrec
    def go(acc: Int, r: RList[T]): Int = r match {
      case RNil   => acc
      case _ :: t => go(1 + acc, t)
    }
    go(0, this)
  }

  // Not tail recursive!!
  def reverse2: RList[T] = {
    @tailrec
    def go(r: RList[T], acc: RList[T] => RList[T]): RList[T] = r match {
      case RNil => acc(RNil)
      case h :: t => go(t, h :: acc(_))
    }
    go(this, identity)
  }

  // reverse the list
  def reverse: RList[T] = {
    @tailrec
    // two equations:
    // go(RNil  , acc) => acc
    // go(h :: t, acc) => go(t, h :: acc)
    def go(r: RList[T], acc: RList[T]): RList[T] = r match {
      case RNil   =>            acc
      case h :: t => go(t, h :: acc)
    }
    go(this, RNil)
    // go(1 :: 2 :: 3 :: RNil,   RNil) ==
    // go(2 :: 3 :: RNil,   1 :: RNil) ==
    // go(3 :: RNil,   2 :: 1 :: RNil) ==
    // go(RNil,   3 :: 2 :: 1 :: RNil) ==
    //            3 :: 2 :: 1 :: RNil
  }

  // complexity:
  // O(this.length) + O(reverse)
  // O(this.length) + O(this.length)
  // 2 * O(this.length)
  // O(this.length)
  // concatenate another list to this one
  def ++[S >: T](other: RList[S]): RList[S] = {
    // go(RNil, acc) => acc
    // go(h :: t, acc) => go(t, h :: acc)
    @tailrec
    def go(r: RList[S], acc: RList[S]): RList[S] = r match {
      case RNil   => acc
      case h :: t => go(t, h :: acc)
    }

    go(this.reverse, other)

  }

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T] = {
    // go(_,     acc,     []) = acc.reverse
    // go(0,     acc, _ :: t) = acc.reverse ++ t
    // go(k + 1, acc, h :: t) = go(k, h :: acc, t)

    // go(2, [], [a,b,c,d]) =
    // go(1, [a], [b, c, d]) =
    // go(0, [b, a], [c, d]) =
    // [b, a].reverse ++ [d] =
    @tailrec
    def go(i: Int, acc: RList[T], rem: RList[T]): RList[T] = rem match {
      case RNil             => acc.reverse
      case _ :: t if i <= 0 => acc.reverse ++ t
      case h :: t           => go(i - 1, h :: acc, t)
    }
    go(index, RNil, this)
  }

  // the big 3
  // Complexity:
  // O(this.length) * O(f)
  def map[S](f: T => S): RList[S] = {
    @tailrec
    def map_(rem: RList[T], acc: RList[S]): RList[S] = rem match {
      case RNil   => acc.reverse
      case h :: t => map_(t, f(h) :: acc)
    }
    map_(this, RNil)
  }

  // Complexity:
  // O(Z^2)
  def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMap_(rem: RList[T], acc: RList[S]): RList[S] = rem match
      case RNil => acc.reverse
      case h :: t => flatMap_(t, f(h) ++ acc)

    @tailrec
    def concatenateAll(xss: RList[RList[S]], cur: RList[S], acc: RList[S]): RList[S] =
      (xss, cur) match
        case (RNil, RNil)     => acc
        case (hh :: tt, RNil) => concatenateAll(tt, hh, acc)
        case (_, h :: t)      => concatenateAll(xss, t, h :: acc)

    @tailrec
    def flatMap1_(rem: RList[T], acc: RList[RList[S]]): RList[S] = rem match
      case RNil => concatenateAll(acc, RNil, RNil)
      case h :: t => flatMap1_(t, f(h).reverse :: acc)

    flatMap_(this, RNil)
  }


//  def flatMap1[S](f: T => RList[S]): RList[S] =
//    @tailrec def flatMap_(rem: RList[T], acc: RList[RList[S]]): RList[S] = rem match
//      case RNil   => acc.reverse
//      case h :: t => flatMap_(t, f(h) :: acc)


  def filter(pred: T => Boolean): RList[T] = {
    @tailrec
    def go(acc: RList[T], rem: RList[T]): RList[T] = rem match {
      case RNil              => acc.reverse
      case h :: t if pred(h) => go(h :: acc, t)
      case _ :: t            => go(     acc, t)
    }
    go(RNil, this)
  }

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  def rle: RList[(T, Int)] = {

    @tailrec
    def go(rem: RList[T], acc: RList[(T, Int)]): RList[(T, Int)] = (rem, acc) match {
      case (RNil  , _               )            => acc.reverse
      case (h :: t, (h0, n) :: acctT) if h == h0 => go(t, (h, n + 1) :: acctT)
      case (h :: t, _               )            => go(t, (h,     1) :: acc  )
    }

    go(this, RNil)
  }

  // duplicate
  def duplicateEach0(k: Int): RList[T] = {
    def generate(k: Int)(t: T): RList[T] = from(Iterable.fill(k)(t))
    flatMap(generate(k))
  }

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T] = {
    // equations
    // go(3, h :: t, acc) => go(2, h :: t, h :: acc)
    // go(2, h :: t, acc) => go(1, h :: t, h :: acc)
    // go(1, h :: t, acc) => go(0, h :: t, h :: acc)
    // go(0, h :: t, acc) => go(k, t, acc)
    // go(_, [],     acc) => acc.reverse

    @tailrec
    def go(n: Int, rem: RList[T], acc: RList[T]): RList[T] = (n, rem) match {
      case (_, RNil  ) => acc.reverse
      case (0, _ :: t) => go(k, t, acc)
      case (_, h :: _) => go(n - 1, rem, h :: acc)
    }

    go(k, this, RNil)
  }

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T] = {
    // go(0, rem,  acc) => rem ++ acc.reverse
    // go(_, [],  acc)  => acc.reverse
    // go(k + 1, h :: t,  acc)  => go(k, t, h :: acc)
    @tailrec
    def go(i: Int, rem: RList[T], acc: RList[T]): RList[T] =
      (i, rem) match {
        case (0, RNil  ) => this
        case (0, _     ) => rem ++ acc.reverse
        case (_, RNil  ) => acc.reverse
        case (_, h :: t) => go(i - 1, t, h :: acc)
      }
    go(k % length, this, RNil)
  }

  // random sample
  def sample(k: Int): RList[T] = {
   @tailrec
   def go(i: Int, acc: RList[T]): RList[T] =
     if i <= 0 || this.isEmpty
     then
       acc
     else
      go(i - 1, this.apply(Random.nextInt(this.length)) :: acc)

    go(k, RNil)

    RList.from(List.fill(k)(this.apply(Random.nextInt(this.length))))
    RList.from((1 to k).map(_ => Random.nextInt(this.length)).map(apply))
  }

  def sorted[S >: T](ordering: Ordering[S]): RList[S] = {

    ???
  }
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(i: Int): Nothing = throw new NoSuchElementException

}

case class ::[+T](
  override val head: T,
  override val tail: RList[T]
) extends RList[T] {

  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(i: Int): T = {
    @tailrec
    def go(l: RList[T], j: Int): T = {
      if (j <= 0) l.head
      else go(l.tail, j - 1)
    }
    go(this, i)
  }
}

object RList {
  def from[T](it: Iterable[T]): RList[T] =
    it.foldRight[RList[T]](RNil)(_ :: _)

}

object ListProblems extends App {

  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)
  val oneToTen = RList.from(1 to 10)

  def testEasyFunctions() = {
    // test get-kth
    println(aSmallList.apply(0))
    println(aSmallList.apply(2))
    println(aLargeList.apply(8735))

    // test length
    println(aSmallList.length)
    println(aLargeList.length)

    // test reverse
    println(aSmallList.reverse)
    println(aLargeList.reverse)

    // test concat
    println(aSmallList ++ aLargeList)

    // test removeAt
    println(aLargeList.removeAt(13))

    // map
    println(aLargeList.map(x => 2 * x))
    // flatMap
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil) // 1.3 seconds!
    println(System.currentTimeMillis() - time)
    // filter
    println(aLargeList.filter(x => x % 2 == 0))
  }

  /**
    * Medium difficulty functions
    */
  def testMediumDifficultyFunctions() = {
    // run-length encoding
    println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)
    // duplicateEach
    println(aSmallList.duplicateEach(4))
    // rotate
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))

  }

//  testMediumDifficultyFunctions()
//  testEasyFunctions()
//  println(aLargeList.sample(1000))
  println(aSmallList.foldLeft(1)( _ * _))
}

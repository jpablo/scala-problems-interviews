package com.rockthejvm.lists

import com.rockthejvm.lists.RList.from

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(i: Int): T

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
    def flatMap_(rem: RList[T], acc: RList[S]): RList[S] = rem match {
      case RNil   => acc.reverse
      case h :: t => flatMap_(t, f(h) ++ acc)
    }
    flatMap_(this, RNil)
  }

  def filter(pred: T => Boolean): RList[T] = {
    @tailrec
    def go(acc: RList[T], rem: RList[T]): RList[T] = rem match {
      case RNil              => acc.reverse
      case h :: t if pred(h) => go(h :: acc, t)
      case _ :: t            => go(     acc, t)
    }
    go(RNil, this)
  }

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

}

object RList {
  def from[T](it: Iterable[T]): RList[T] =
    it.foldRight[RList[T]](RNil)(_ :: _)


}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
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
  override def apply(i: Int): T = {
    @tailrec
    def go(l: RList[T], j: Int): T = {
      if (j <= 0) l.head
      else go(l.tail, j - 1)
    }
    go(this, i)
  }
}

object ListProblems extends App {

  val smallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val largeList = RList.from(1 to 10000)

  assert( smallList(0) == 1 )
  assert( smallList(1) == 2 )
  assert( smallList(2) == 3 )

  assert( smallList.length == 3 )

  assert( smallList.reverse  == (3 :: 2 :: 1 :: RNil) )
  assert( smallList.reverse2 == (3 :: 2 :: 1 :: RNil) )
  assert( RNil.reverse == RNil )

  assert(largeList.reverse(9999) == 1)
  assert(((1 :: 2 :: RNil) ++ smallList) == 1 :: 2 :: 1 :: 2 :: 3 :: RNil)

  assert( smallList.removeAt(0) == (2 :: 3 :: RNil) )

  assert( RNil.map((_: Int) + 1) == RNil )
  assert( smallList.map(_ + 1) == (2 :: 3 :: 4 :: RNil) )

  assert( smallList.flatMap(i => i :: i :: RNil) == (1 :: 1 :: 2 :: 2 :: 3 :: 3 :: RNil) )

  assert( smallList.filter(_ % 2 == 0) == (2 :: RNil) )
  assert( smallList.filter(_ % 2 == 1) == (1 :: 3 :: RNil) )

  assert( (RNil).rle == RNil  )
  assert( (1 :: RNil).rle == (1, 1) :: RNil  )
  assert( (1 :: 1 :: 2 :: 3 :: 3 :: RNil).rle == (1, 2) :: (2, 1) :: (3, 2) :: RNil  )

  assert( smallList.duplicateEach(2) == 1 ::1 :: 2 :: 2 :: 3 :: 3 :: RNil )

  println( smallList.rotate(0) )
  println( smallList.rotate(1) )
  println( smallList.rotate(2) )
  println( smallList.rotate(3) )

  println( smallList.rotate(4) )
  println( smallList.rotate(5) )
  println( smallList.rotate(6) )
}

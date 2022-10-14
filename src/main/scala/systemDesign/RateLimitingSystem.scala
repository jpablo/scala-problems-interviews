package systemDesign

import zio.json.*
import scala.collection.mutable

class RateLimiter(_n: Int, _t: Int) {
  type TimeStamp = Int
  val queue: mutable.Queue[TimeStamp] = mutable.Queue.empty

  def shouldAllow(current: TimeStamp): Boolean = {
    println(queue)
    while (queue.nonEmpty && queue.head <= current - _t)
      queue.dequeue
    if (queue.size < _n) {
      queue += current
      true
    } else
      false
  }
}

/**
 * Your RateLimiter object will be instantiated and called as such:
 * var obj = new RateLimiter(n, t)
 * var param_1 = obj.shouldAllow(timestamp)
 */


@main def mainRateLimiter =

  enum Instructions:
    case RateLimiter, shouldAllow

  object Instructions:
    given JsonDecoder[Instructions] = JsonDecoder[String].map(Instructions.valueOf)


  val input = """["RateLimiter","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow","shouldAllow"]"""
    .fromJson[List[Instructions]].right.get
  
  val data = """[[16,12],[38],[42],[48],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50],[50]]"""
    .fromJson[List[List[Int]]].right.get

  val expected = """[false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false]"""
    .fromJson[List[Boolean]].right.get

  var rateLimiter: RateLimiter = null

  input.zip(data).zip(expected).foreach { case ((i, d), e) =>
    val out = 
      i match
        case Instructions.RateLimiter => 
          rateLimiter = new RateLimiter(d(0), d(1))
        case Instructions.shouldAllow =>
          rateLimiter.shouldAllow(d(0))
    
    println((i, d, out, e))
  }
  
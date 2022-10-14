package puzzles


type Matrix[A] = List[List[A]]
type Row[A]    = List[A]

def transpose [A] (m: Matrix[A]): Matrix[A] =
  def first (r: Row[A], m: Matrix[A]): Matrix[A] = (r, m) match
    case (Nil, m)           => m
    case (a :: as, Nil)     => (a :: Nil) :: first (as, Nil)
    case (a :: as, b :: bs) => (a :: b)   :: first (as, bs)
  m match
    case Nil => Nil
    case r :: rs => first (r, transpose(rs))


@main def mainTranpose =
  val m = 
    List(
      List(1, 2),
      List(3, 4),
    )
  val t = transpose(m)
  t.foreach(println)

  

package trees


enum Tree [A]:
  case Leaf (a: A)
  case Bin (l: Tree[A], r: Tree[A])


trait Functor [F[_]]:
  def fmap [A, B] : (A => B) => F[A] => F[B]


trait Monad [M[_] : Functor]:
  def pure [A] : A => M[A]
  def flatMap [A, B] : M[A] => (A => M[B]) => M[B]


def mapM [M[_]: Monad, A, B] : (A => M[B]) => (List[A] => M[List[B]]) = ???


trait Applicative [M[_] : Functor]:
  def pure [A]  : A => M[A]
  def ap [A, B] : M[A => B] => M[A] => M[B]


given [M[_]] (using m: Monad[M], f: Functor[M]) : Applicative [M] with
  def pure [A]  : A => M[A] = m.pure
  def ap [A, B] : M[A => B] => M[A] => M[B] = 
    f => x =>
      m.flatMap(f)(g => m.flatMap(x)(y => m.pure(g(y))))



// trait Travesable[T[_]](using func: Functor[T]):
//   def traverse [M[_]: Applicative, A, B] : (A => M[B]) => T[A] => M[T[B]] =
//     f => sequence(func.fmap(f))
//   def sequence [M[_]: Applicative, A] : T[M[A]] => M[T[A]] =
//     tma => traverse(x => x)



@main def mainTaversable =
  println(1)
  
  

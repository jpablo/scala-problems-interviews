package puzzles

extension [A](a: A)
  infix def in(sa: Set[A]): Boolean =
    sa.contains(a)

  infix def in[V](sa: collection.Map[A, V]): Boolean =
    sa.contains(a)

  infix def in(sa: List[A]): Boolean =
    sa.contains(a)

  infix def in(sa: Seq[A]): Boolean =
    sa.contains(a)

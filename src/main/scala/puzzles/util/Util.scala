package puzzles.util


extension [A](a: => A)
  def time: (A, Long) =
    val t = System.nanoTime
    // val t = System.currentTimeMillis()
    val a1 = a 
    (a1, System.nanoTime - t)

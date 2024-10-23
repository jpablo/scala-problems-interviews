package algorithms

import puzzles.leetCode.TreeNodeGeneric

import scala.annotation.tailrec

type Tree[A] = TreeNodeGeneric[A] | Null

case class PreorderData[Value, Accumulated, Pending](
    nextPending:   (TreeNodeGeneric[Value], Accumulated) => Pending,
    nextAcc:       (TreeNodeGeneric[Value], Accumulated) => Accumulated,
    backtrackRoot: Pending => Tree[Value],
    backtrackAcc:  (Pending, Accumulated) => Accumulated
)

@tailrec
def preorderLoop[Value, Accumulated, Pending](
    root:    Tree[Value],
    pending: List[Pending],
    acc:     Accumulated,
    data:    PreorderData[Value, Accumulated, Pending]
): Accumulated =
  import data.*
  (root, pending) match
    // process node
    case (n: TreeNodeGeneric[_], _) =>
      preorderLoop(root = n.left, pending = nextPending(n, acc) :: pending, acc = nextAcc(n, acc), data)

    // backtracking
    case (null, p :: tail) =>
      preorderLoop(root = backtrackRoot(p), pending = tail, acc = backtrackAcc(p, acc), data)

    // done
    case (null, Nil) => acc

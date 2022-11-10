package systemDesign

import scala.language.unsafeNulls

class ConsistentHashing(_initialNodes: Int) {
  type Key = Int
  type Node = Int
  type Hash = Int
  var maxNode = _initialNodes

  var nodes: Map[Node, Vector[Key]] = 
    (1 to _initialNodes).map(i => i -> Vector.empty).toMap

  def random[K, V](s: Map[K, V]): (K, V) = {
    val n = util.Random.nextInt(s.size)
    s.iterator.drop(n).next
  } 

  def chooseNode: (Node, Vector[Key]) =
    random(nodes)

  def getNodeForKey(key: Key): Node = {
    val found = nodes.filter { case (n, ks) => ks.contains(key) }.keys
    if (found.isEmpty) {
      val (node2, keys2) = chooseNode
      nodes += (node2 -> (keys2 :+ key))
      node2
    } else
      found.head
  }

  def removeNode(node: Node): Int = {
    val keys = nodes(node)
    nodes -= node
    // reassign to another node. Find a good way to distribute.
    val (node2, keys2) = chooseNode
    nodes += (node2 -> (keys2 ++ keys))
    node2
  }

  def addNode(): Array[Node] = {
    maxNode += 1
    val newNode = maxNode
    val (node, keys) = chooseNode
    // initialize node1 with the keys from node2
    nodes += (newNode -> keys)
    Array(newNode, node)
  }

  def getKeysInNode(node: Node): List[Key] = {
    nodes(node).toList
  }
}

/**
 * Your ConsistentHashing object will be instantiated and called as such:
 * var obj = new ConsistentHashing(initialNodes)
 * var param_1 = obj.getNodeForKey(key)
 * var param_2 = obj.removeNode(node)
 * var param_3 = obj.addNode()
 * var param_4 = obj.getKeysInNode(node)
 */

 @main def mainCH =
  enum Instructions:
    case addNode, getKeysInNode, removeNode, getNodeForKey

  import Instructions.*


  val instructions = List(
    addNode,getKeysInNode,getKeysInNode,getKeysInNode,removeNode,getNodeForKey,getNodeForKey,addNode,getKeysInNode,removeNode,getKeysInNode,getKeysInNode,addNode,getKeysInNode,getKeysInNode,addNode,addNode,getNodeForKey,getNodeForKey,getNodeForKey,removeNode,addNode,getKeysInNode,getKeysInNode,addNode,removeNode,getKeysInNode,removeNode,addNode,addNode,addNode,getKeysInNode,getNodeForKey,addNode,getKeysInNode,addNode,getNodeForKey,getNodeForKey,getNodeForKey,getNodeForKey,addNode,getKeysInNode,removeNode,addNode,addNode,getNodeForKey,removeNode,getKeysInNode,addNode,removeNode,removeNode,getKeysInNode,getKeysInNode,getKeysInNode,getKeysInNode,addNode,addNode,getKeysInNode,getKeysInNode,removeNode,addNode,getKeysInNode,getKeysInNode,addNode,addNode,getNodeForKey,getKeysInNode,getNodeForKey,addNode,getNodeForKey,removeNode,addNode,getKeysInNode,addNode,getNodeForKey,getKeysInNode,addNode,removeNode,addNode,addNode,getNodeForKey,getNodeForKey,getKeysInNode,getKeysInNode,addNode,getKeysInNode,addNode,addNode,addNode,removeNode,addNode,addNode,getKeysInNode,addNode,getNodeForKey,getKeysInNode,addNode,getNodeForKey
  )
  val data = List[Integer](
    null,11,1,10,5,214,418,null,2,12,11,6,null,11,2,null,null,724,393,625,11,null,1,16,null,13,6,3,null,null,null,1,613,null,19,null,717,497,977,740,null,14,19,null,null,466,10,2,null,2,24,8,4,22,23,null,null,27,21,20,null,9,18,null,null,336,21,719,null,64,1,null,28,null,984,30,null,18,null,null,948,699,36,33,null,37,null,null,null,4,null,null,8,null,146,32,null,403
  )
  val ch = ConsistentHashing(10)
  for (i, d) <- instructions.zip(data) do
    val r = i match
      case Instructions.addNode       => ch.addNode().toList
      case Instructions.getKeysInNode => ch.getKeysInNode(d)
      case Instructions.removeNode    => ch.removeNode(d)
      case Instructions.getNodeForKey => ch.getNodeForKey(d)
    println(r)
    
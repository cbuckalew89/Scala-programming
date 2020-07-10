//     Binary search trees (dictionaries)
//
//     scala> End.addValue(2)
//     res0: Node[Int] = T(2 . .)
//     
//     scala> res0.addValue(3)
//     res1: Node[Int] = T(2 . T(3 . .))
//     
//     scala> res1.addValue(0)
//     res2: Node[Int] = T(2 T(0 . .) T(3 . .))
//
//     scala> Tree.fromList(List(3, 2, 5, 7, 1))
//     res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
//
//     scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
//     res4: Boolean = true
//     
//     scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
//     res5: Boolean = false

sealed abstract class Tree[+T] {
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def addValue[U >: T <% Ordered[U]](x: U) =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))
}

case object End extends Tree[Nothing] {
  def addValue[U <% Ordered[U]](x: U) = Node(x)
}

object Tree {
  def fromList[T <% Ordered[T]](l: List[T]): Tree[T] = 
    l.foldLeft(End: Tree[T])((r, e) => r.addValue(e))
}

//     A method layoutBinaryTree that turns a tree of normal Nodes into a
//     tree of PositionedNodes
//
//     scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
//     res0: PositionedNode[Char] = T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))
// ******Method 1*******
//
sealed abstract class Tree[+T] {
  def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
    val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
    val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
    (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
  }
}

case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)
}
//
//******Method 2******
//
sealed abstract class Tree[+T] {
  def treeDepth: Int
  def leftmostNodeDepth: Int
  def layoutBinaryTree2: Tree[T] = {
    val d = treeDepth
    val x0 = (2 to leftmostNodeDepth).map((n) => Math.pow(2, d - n).toInt).reduceLeft(_+_) + 1
    layoutBinaryTree2Internal(x0, 1, d - 2)
  }
  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def treeDepth: Int = (left.treeDepth max right.treeDepth) + 1
  def leftmostNodeDepth: Int = left.leftmostNodeDepth + 1
  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T] =
    PositionedNode(
      value,
      left.layoutBinaryTree2Internal(x - Math.pow(2, exp).toInt, depth + 1, exp - 1),
      right.layoutBinaryTree2Internal(x + Math.pow(2, exp).toInt, depth + 1, exp - 1),
      x, depth)
}

case object End extends Tree[Nothing] {
  def treeDepth: Int = 0
  def leftmostNodeDepth: Int = 0
  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int) = End
}
//
//*******Method 3********
//
sealed abstract class Tree[+T] {
  def bounds: List[(Int,Int)]
  def layoutBinaryTree3: Tree[T] = 
    layoutBinaryTree3Internal(bounds.map(_._1).reduceLeft(_ min _) * -1 + 1, 1)
  def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def bounds: List[(Int,Int)] = {
    def lowerBounds = (left.bounds, right.bounds) match {
      case (Nil, Nil) => Nil
      case (lb, Nil)  => lb.map((b) => (b._1 - 1, b._2 - 1))
      case (Nil, rb)  => rb.map((b) => (b._1 + 1, b._2 + 1))
      case (lb, rb) => {
        val shift = lb.zip(rb).map((e) => (e._1._2 - e._2._1) / 2 + 1).reduceLeft(_ max _)
        lb.map(Some(_)).zipAll(rb.map(Some(_)), None, None).map(_ match {
          case (Some((a, b)), Some((c, d))) => (a - shift, d + shift)
          case (Some((a, b)), None)         => (a - shift, b - shift)
          case (None, Some((c, d)))         => (c + shift, d + shift)
          case (None, None) => throw new Exception  // Placate the compiler; can't get here.
        })
      }
    }
    (0, 0) :: lowerBounds
  }
  def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T] = bounds match {
    case _ :: (bl, br) :: _ => PositionedNode(
      value, left.layoutBinaryTree3Internal(x + bl, depth + 1),
      right.layoutBinaryTree3Internal(x + br, depth + 1), x, depth)
    case _ => PositionedNode(value, End, End, x, depth)
  }
}

case object End extends Tree[Nothing] {
  def bounds: List[(Int,Int)] = Nil
  def layoutBinaryTree3Internal(x: Int, depth: Int) = End
}

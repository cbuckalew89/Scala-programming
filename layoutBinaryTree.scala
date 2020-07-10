//     A method layoutBinaryTree that turns a tree of normal Nodes into a
//     tree of PositionedNodes
//
//     scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
//     res0: PositionedNode[Char] = T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))

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

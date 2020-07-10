//     A string representation of binary trees
//
//     scala> Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString
//     res0: String = a(b(d,e),c(,f(g,)))
//
//     scala> Tree.fromString("a(b(d,e),c(,f(g,)))")
//     res1: Node[Char] = a(b(d,e),c(,f(g,)))

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = (left, right) match {
    case (End, End) => value.toString
    case _ => value.toString + "(" + left + "," + right + ")"
  }
}

case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
  override def toString = (left, right) match {
    case (End, End) => value + "[" + x + "," + y + "]"
    case _ => value + "[" + x + "," + y + "](" + left + "," + right + ")"
  }
}

case object End extends Tree[Nothing] {
  override def toString = ""
}

object Tree {
  def string2Tree(s: String): Tree[Char] = {
    def extractTreeString(s: String, start: Int, end: Char): (String,Int) = {
      def updateNesting(nesting: Int, pos: Int): Int = s(pos) match {
        case '(' => nesting + 1
        case ')' => nesting - 1
        case _   => nesting
      }
      def findStringEnd(pos: Int, nesting: Int): Int = 
        if (s(pos) == end && nesting == 0) pos
        else findStringEnd(pos + 1, updateNesting(nesting, pos))
      val strEnd = findStringEnd(start, 0)
      (s.substring(start, strEnd), strEnd)
    }
    s.length match {
      case 0 => End
      case 1 => Node(s(0))
      case _ => {
        val (leftStr, commaPos) = extractTreeString(s, 2, ',')
        val (rightStr, _) = extractTreeString(s, commaPos + 1, ')')
        Node(s(0), string2Tree(leftStr), string2Tree(rightStr))
      }
    }
  }
}

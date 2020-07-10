 Decode a run-length encoded list.
//     Given a run-length code list construct its uncompressed version.
//
//     Example:
//     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

object Decode {
  def decode[A](ls: List[(Int, A)]): List[A] =
    ls flatMap { e => List.make(e._1, e._2) }
}

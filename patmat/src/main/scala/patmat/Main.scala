package patmat

object Main extends App {
  import Huffman._
  val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
  val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

  val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
  println(decode(t1, encode(t1)("ab".toList)) == "ab".toList)
}


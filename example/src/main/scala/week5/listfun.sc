object listfun {
  val data = List("a", "a", "a", "b", "b", "c", "a")

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, second) = xs.span(e => e == x)
      first :: pack(second)
  }
  pack(data)

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(y => (y.head, y.length))

  encode(data)

  def mapFunc[T, U](xs: List[T], f:T => U): List[U] =
    (xs foldRight List[U]())((x, xs) => f(x) :: xs)

  def lengthFunc[T](xs: List[T]): Int =
    (xs foldRight 0)((_, len) => len + 1)
}
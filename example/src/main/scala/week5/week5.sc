object week5 {
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 3, 5)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  val empty = List()

  def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case x :: xs => x match {
      case list: List[Any] => flatten(list) ++ flatten(xs)
      case elem: Any => elem :: flatten(xs)
    }
  }
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n <= 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (lt(x ,y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
        }
      val (fst, snd) = xs splitAt(n)
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }

  val ints = List(2, 1, -5 , 3 ,8, 12)
  msort(ints)((x, y) => x < y)
  msort(fruit)((x, y ) => x.compareTo(y) < 0)
}
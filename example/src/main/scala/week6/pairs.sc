object pairs {
  def isPrime(n: Int) = (2 to math.sqrt(n).intValue()) forall (n % _ != 0)

  val n = 7
  (1 until n) flatMap(i =>
    (1 until i) map (j => (i,j))) filter (pair => isPrime(pair._1 + pair._2))


  def scalarProduct(xs: List[Double], ys: List[Double]) :Double =
    (for ((x, y) <- xs zip ys) yield x*y).sum
}
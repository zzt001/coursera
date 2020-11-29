import sun.invoke.empty.Empty

object intsets {
  val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
  val b: Array[IntSet] = a
  b(0) = new Empty
  val s: NonEmpty = a(0)
}

abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  override def contains(x: Int) = false

  override def union(other: IntSet): IntSet = other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  override def union(other: IntSet): IntSet = {
    left.union(right.union(other.incl(elem)))
  }

  override def contains(x: Int) = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def toString = "{" + left + elem + right + "}"
}




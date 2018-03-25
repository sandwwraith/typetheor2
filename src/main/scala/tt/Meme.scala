package tt

import scala.collection.{mutable => m}

class Meme[-T, +R](fn: T => R) extends (T => R) {
  private[this] val cache = m.HashMap.empty[T, R]

  def apply(x: T): R = cache getOrElseUpdate(x, fn(x))
}

object memoize {
  def apply[T, R](fn: T => R): Meme[T, R] = new Meme(fn)
}


object FibTest {
  lazy val fib: Int => Int = memoize {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 1) + fib(n - 2)
  }
}

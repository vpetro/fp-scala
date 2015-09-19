package org.fpscala.chapter5

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def helper(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc
      case Cons(h, t) => helper(stream, h() :: acc)
    }
    helper(this, List.empty[A])
  }

  def drop(n: Int): Stream[A] = {
    def helper(n: Int, stream: Stream[A]): Stream[A] = stream match {
      case Empty => Empty
      case Cons(_, _) if (n == 0) => stream
      case Cons(_, t) => helper(n - 1, t())
    }
    helper(n, this)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if (n == 0) => Empty
    case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      lazy val head = h()
      if (f(head)) Stream.cons(head, t().takeWhile(f)) else Empty
    }
    case _ => Empty
  }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      lazy val head = h()
      if (f(head)) Stream.cons(head, t().takeWhile(f)) else Empty
    }
    case _ => Empty
  }

  def takeWhileFoldRight(f: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((i, z) => if (f(i)) Stream.cons(i, z) else Empty)

  def headOptionFoldRight: Option[A] =
    this.foldRight[Option[A]](None)((i, z) => Some(i))

  def forAll(f: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => f(h()) && t().forAll(f)
  }

  def map[B](f: A => B): Stream[B] =
    this.foldRight[Stream[B]](Empty)((i, z) => Stream.cons(f(i), z))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((i, z) => if (f(i)) Stream.cons(i, z) else z)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    this.foldRight(other)((i, z) => Stream.cons(i, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight[Stream[B]](Empty)((i, z) => f(i).append(z))

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] = Stream.cons(first, go(second, first + second))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold[Stream[A]](Empty) { case (a, s) => Stream.cons(a, unfold(s)(f)) }
  }

}

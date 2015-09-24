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
      case Cons(h, t) => helper(t(), acc :+ h())
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

  def takeUnfold(n: Int): Stream[A] =
    Stream.unfold((n, this)) { case (counter, stream) => 
      if (counter == 0) None else stream.headOption.map(v => (v, (counter - 1, stream.drop(1))))
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

  def takeWhileUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this)(s => s.headOption.flatMap(v =>
      if (f(v)) Some(v, s.drop(1)) else None
    ))

  def takeWhileFoldRight(f: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((i, z) => if (f(i)) Stream.cons(i, z) else Empty)

  def zipWith[B](other: Stream[B]): Stream[(A, B)] =
    Stream.unfold((this, other)) { case (s1, s2) =>
      s1.headOption.flatMap(v1 => s2.headOption.map(v2 => ((v1, v2), (s1.drop(1), s2.drop(1)))))
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, other)) { 
      case (Empty, Empty) => None
      case (s1, s2) => Some((s1.headOption, s2.headOption), (s1.drop(1), s2.drop(1)))
    }


  def headOptionFoldRight: Option[A] =
    this.foldRight[Option[A]](None)((i, z) => Some(i))

  def forAll(f: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => f(h()) && t().forAll(f)
  }

  def map[B](f: A => B): Stream[B] =
    this.foldRight[Stream[B]](Empty)((i, z) => Stream.cons(f(i), z))

  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(n => n.headOption.map(v => (f(v), n.drop(1))))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((i, z) => if (f(i)) Stream.cons(i, z) else z)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    this.foldRight(other)((i, z) => Stream.cons(i, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight[Stream[B]](Empty)((i, z) => f(i).append(z))

  def startsWith[B >: A](other: Stream[B]): Boolean =
    this.zipWith(other).foldRight(true)((i, z) => z && (i._1.equals(i._2)))

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)(n => n.headOption.map(_ => (n.drop(1), n.drop(1))))

  def scanRight1[B >: A](z: => B)(f: (A, => B) => B): Stream[B] =
    // this doesn't use intermediate results
    Stream.unfold(this)(n => n.headOption.map(v => (n.drop(1).foldRight(z)(f), n.drop(1))))


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

  def constantUnfold[A](a: A): Stream[A] =
    Stream.unfold(a)((a: A) => Some((a, a)))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fromUnfold(n: Int): Stream[Int] =
    Stream.unfold(n)((n: Int) => Some((n, n + 1)))

  def fibs: Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] = Stream.cons(first, go(second, first + second))
    go(0, 1)
  }

  def fibsUnfold: Stream[Int] = {
    Stream.unfold((0, 1)){ case (first, second) => Some(first, (second, first + second)) }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold[Stream[A]](Empty) { case (a, s) => Stream.cons(a, unfold(s)(f)) }
  }

}

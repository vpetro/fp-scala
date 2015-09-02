package org.fpscala.chapter3

import scala.collection.immutable.{List => _, Nil => _}
import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
    val example = Cons(1, Cons(2, Cons(3, Nil)))

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def drop[A](lst: List[A], n: Int): List[A] =
    if (n == 0) lst else drop(tail(lst), n - 1)

  def dropWhile[A](lst: List[A])(predicate: A => Boolean): List[A] = lst match {
    case Cons(h, t) if (predicate(h)) => dropWhile(t)(predicate)
    case _ => lst
  }

  def setHead[A](lst: List[A], head: A): List[A] =
    Cons(head, tail(lst))

  def init[A](lst: List[A]): List[A] =  lst match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case Nil => Nil
  }

  // problem 7
  // Given the way it is currently implemented, it is not possible.

  // problem 8
  // The original list is returned.

  def length[A](lst: List[A]): Int =
    foldRight(lst, 0)((_, z) => z + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFl(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productFl(ints: List[Int]): Int = foldLeft(ints, 1)(_ * _)

  def lengthFl[A](lst: List[A]): Int = foldLeft(lst, 0)((i, _) => i + 1)

  def reverse[A](lst: List[A]): List[A] = foldLeft[A, List[A]](lst, Nil)((t, h) => Cons(h, t))

  def append[A](first: List[A], second: List[A]): List[A] = first match {
    case Nil => second
    case Cons(h, t) => Cons(h, append(t, second))
  }

  def append2[A](first: List[A], second: List[A]): List[A] =
    foldRight(first, second)(Cons(_, _))

  def concat[A](lst: List[List[A]]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, y) => foldLeft(y, x)(append(_, _))
  }

  def add1(lst: List[Int]): List[Int] = lst match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  def add1fold(lst: List[Int]): List[Int] =
    foldRight[Int, List[Int]](lst, Nil)((a, z) => Cons(a + 1, z))

  def toString(lst: List[Double]): List[String] =
    foldRight[Double, List[String]](lst, Nil)((a, z) => Cons(a.toString, z))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](as, Nil)((a, z) => Cons(f(a), z))

  def filter[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case Cons(h, t) if (f(h)) => Cons(h, filter(t)(f))
  }

  def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] = 
    concat(map(lst)(f))

  def filterFM[A](lst: List[A])(f: A => Boolean): List[A] =
    flatMap(lst)((a => if (f(a)) Cons(a, Nil) else Nil))

  def sumLists(first: List[Int], second: List[Int]): List[Int] = (first, second) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumLists(t1, t2))
  }

  def zipWith[A](first: List[A], second: List[A])(f: (A, A) => A): List[A] = (first, second) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequences[A](lst: List[A]): List[List[A]] = {
    def helper[A](input: List[A], last: List[A], output: List[List[A]]): List[List[A]] = input match {
      case Nil => output
      case Cons(x, xs) => helper(xs, Cons(x, last), Cons(Cons(x, last), output))
    }

    val result = lst match {
      case Nil => Nil
      case Cons(x, xs) => helper(xs, Cons(x, Nil), Cons(Cons(x, Nil), Nil))
    }

    reverse(map(result)(reverse))
  }

}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    def helper[A](tree: Tree[A], current: Int): Int = tree match {
      case Leaf(_) => current + 1
      case Branch(l, r) => helper(l, 0) + helper(r, 0) + current + 1
    }
    helper(tree, 0)
  }

  def sizeFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => l + r + 1)

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def maximumFold(tree: Tree[Int]): Int =
    Tree.fold(tree)(v => v)((l, r) => l.max(r))

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(x) => 1
    case Branch(l, r) => depth(l).max(depth(r)) + 1
  }

  def depthFold[A](tree: Tree[A]): Int = 
    Tree.fold(tree)(_ => 1)((l, r) => l.max(r) + 1)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(v => Leaf(f(v)))(Branch(_, _))

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}


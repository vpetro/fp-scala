package org.fpscala.chapter4

import scala.{Option => _, Some => _, None => _}
import scala.{Either => _, Left => _, Right => _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =  this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if (f(v)) Some(v) else None
  }

  def fold[B](default: => B)(f: A => B): B = this match {
    case None => default
    case Some(v) => f(v)
  }



}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionOps {

  def lift[A, B](f: A => B) : Option[A] => Option[B] = x => x.map(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))


  def sequence[A](lst: List[Option[A]]): Option[List[A]] =  {
    lst.foldLeft[Option[List[A]]](Some(List.empty[A]))((z, i) => map2(z, i)((a, b) => a :+ b))
  }

  def sequenceTraverse[A](lst: List[Option[A]]): Option[List[A]] =
    traverse(lst)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft[Option[List[B]]](Some(List.empty[B]))((z, i) =>map2(z, f(i))((a, b) => a :+ b))

  }


}

object Variance {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))


}


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(x) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(r1 => b.map(r2 => f(r1, r2)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

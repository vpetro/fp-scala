package org.fpscala.chapter4

import OptionOps._

import org.scalatest.FunSuite
import scala.{Option => _, Some => _, None => _}

class OptionTest extends FunSuite {

  test("map") {
    assert(Some(2) == Some(1).map(_ + 1))
  }

  test("flatMap") {
    assert(Some(2) == Some(1).flatMap(v => Some(v + 1)))
  }

  test("getOrElse") {
    val none: Option[Int] = None
    val some: Option[Int] = Some(2)

    assert(2 == Some(2).getOrElse(0))
    assert(0 == none.getOrElse(0))
  }

  test("orElse") {
    assert(Some(2) == Some(2).orElse(Some(1)))
    assert(Some(1) == None.orElse(Some(1)))
  }

  test("filter") {
    assert(Some(2) == Some(2).filter(_ % 2 == 0))
  }

  test("map2") {
    val a = Some(1)
    val b = Some(2)
    val c: Option[Int] = None
    assert(Some(3) == map2(a, b)(_ + _))
    assert(None == map2(a, c)(_ + _))
  }

  test("sequence") {
    val l: List[Option[Int]] = List(Some(1), Some(1))
    val l2: List[Option[Int]] = List(Some(1), None)
    val l3: List[Option[Int]] = List(None)
    assert(Some(List(1,1)) == sequence(l))
    assert(None == sequence(l2))
    assert(None == sequence(l3))

    assert(Some(List(1,1)) == sequenceTraverse(l))
    assert(None == sequenceTraverse(l2))
    assert(None == sequenceTraverse(l3))
  }

  test("traverse") {
    val l = List(1,2,3)
    def f(a: Int) = if (a % 2 == 0) Some(a) else None
    def f2(a: Int) = Some(a)

    assert(None == traverse(l)(f))
    assert(Some(List(1,2,3)) == traverse(l)(f2))
  }

}

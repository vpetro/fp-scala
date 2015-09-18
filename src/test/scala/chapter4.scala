package org.fpscala.chapter3

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

}

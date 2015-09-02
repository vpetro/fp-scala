package org.fpscala.chapter3

import org.scalatest.FunSuite

class SubsequenceTest extends FunSuite {

  test("subsequence of a empty list is en empty list") {
    val lst: List[Int] = Nil
    assert(Nil == List.hasSubsequences(lst))
  }

  test("subsequence a list with 1 element is a List of 1 element") {
    val lst: List[Int] = List(1)
    assert(List(List(1)) == List.hasSubsequences(lst))
  }

  test("subsequence a list with more than 1 element is a List of more than 1 element") {
    val lst: List[Int] = List(1,2)
    assert(List(List(1), List(1,2)) == List.hasSubsequences(lst))
  }

  test("input list of 3 elements") {
    val lst: List[Int] = List(1,2,3)
    assert(List(List(1), List(1,2), List(1,2,3)) == List.hasSubsequences(lst))
  }
}

class TreeSizeTest extends FunSuite {
  test("leaf tree has size 1") {
    val t: Tree[Int] = Leaf(1)
    assert(1 == Tree.size(t))
    assert(1 == Tree.sizeFold(t))
  }

  test("tree of a branch with two leaves should have size of 3") {
    val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    assert(3 == Tree.size(t))
    assert(3 == Tree.sizeFold(t))
  }

  test("tree of two branches should have size 7") {
    val t: Tree[Int] = Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(Leaf(3), Leaf(4))
    )

    assert(7 == Tree.size(t))
    assert(7 == Tree.sizeFold(t))
  }

}


class TreeMaximumTest extends FunSuite {

  test("leaf tree has size 1") {
    val t: Tree[Int] = Leaf(1)
    assert(1 == Tree.maximum(t))
    assert(1 == Tree.maximumFold(t))
  }

  test("tree of a branch with two leaves should have size of 3") {
    val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    assert(2 == Tree.maximum(t))
    assert(2 == Tree.maximumFold(t))
  }

  test("tree of two branches should have size 7") {
    val t: Tree[Int] = Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(Leaf(3), Leaf(4))
    )
    assert(4 == Tree.maximum(t))
    assert(4 == Tree.maximumFold(t))
  }

}

class TreeDepthTest extends FunSuite {

  test("leaf tree has depth 1") {
    val t: Tree[Int] = Leaf(1)
    assert(1 == Tree.depth(t))
    assert(1 == Tree.depthFold(t))
  }

  test("tree of a branch with two leaves should have depth of 2") {
    val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    assert(2 == Tree.depth(t))
    assert(2 == Tree.depthFold(t))
  }

  test("tree of two branches should have depth 3") {
    val t: Tree[Int] = Branch(
      Leaf(1),
      Branch(Leaf(3), Leaf(4))
    )
    assert(3 == Tree.depth(t))
    assert(3 == Tree.depthFold(t))
  }
}

class TreeMapTest extends FunSuite {

  test("leaf tree has depth 1") {
    val t: Tree[Int] = Leaf(1)
    val e: Tree[Int] = Leaf(2)
    assert(e == Tree.map(t)(_ + 1))
    assert(e == Tree.mapFold(t)(_ + 1))
  }

  test("tree of a branch with two leaves should have depth of 2") {
    val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    val e: Tree[Int] = Branch(Leaf(2), Leaf(3))
    assert(e == Tree.map(t)(_ + 1))
    assert(e == Tree.mapFold(t)(_ + 1))
  }

  test("tree of two branches should have depth 3") {
    val t: Tree[Int] = Branch(
      Leaf(1),
      Branch(Leaf(3), Leaf(4))
    )

    val e: Tree[Int] = Branch(
      Leaf(2),
      Branch(Leaf(4), Leaf(5))
    )

    assert(e == Tree.map(t)(_ + 1))
    assert(e == Tree.mapFold(t)(_ + 1))
  }

}

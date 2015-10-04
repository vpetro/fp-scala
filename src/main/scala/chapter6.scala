package org.fpscala.chapter6



trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (0 > i) ((i + 1) * (-1)) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue.toDouble + 1.0, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def helper(count: Int, rng: RNG, result: List[Int]): (List[Int], RNG) = {
      if (count > 0) {
        val (i, r) = nonNegativeInt(rng)
        helper(count - 1, r, i +: result)
      } else {
        (result, rng)
      }
    }
    helper(count, rng, List.empty[Int])
  }
}

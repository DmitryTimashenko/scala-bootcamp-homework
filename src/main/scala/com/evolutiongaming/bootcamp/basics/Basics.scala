package com.evolutiongaming.bootcamp.basics

object Basics extends App {
  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def gcd(a: Int, b: Int): Option[Int] =
    if (0 == a && 0 == b) None else if (0 == b) Some(Math.abs(a)) else gcd (b, a % b)

  def lcm(a: Int, b: Int): Option[Int] = gcd(a, b).map(Math.abs(a * b) / _);
}
package com.evolutiongaming.bootcamp.basics

object DataStructures {

  //  Running Sum of 1d Array https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0) { (running, current) => running + current }.drop(1)
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.splitAt(n) match {
      case (a, b) => (a zip b).flatMap(pair => Array(pair._1, pair._2))
    }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points/
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xs = points.map(_.head).sorted
    xs.zip(xs.tail).flatMap { case (a, b) => Array(b - a) }.max
  }
}

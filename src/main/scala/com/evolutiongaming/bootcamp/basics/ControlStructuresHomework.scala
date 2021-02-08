package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructuresHomework.Command._

import scala.io.Source

object ControlStructuresHomework {

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  final case class Result(command: Command, result: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.split(" +").toList match {
      case "divide" :: divided :: divisor :: Nil => Right(Divide(divided.toDouble, divisor.toDouble))
      case "sum" :: tail                         => Right(Sum(tail.map(_.toDouble)))
      case "average" :: tail                     => Right(Average(tail.map(_.toDouble)))
      case "min" :: tail                         => Right(Min(tail.map(_.toDouble)))
      case "max" :: tail                         => Right(Max(tail.map(_.toDouble)))
      case _                                     => Left(ErrorMessage("Error: Command was not parsed"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(_, 0)              => Left(ErrorMessage("Error: Division by zero"))
      case Divide(dividend, divisor) => Right(Result(x, dividend / divisor))
      case Sum(numbers)              => Right(Result(x, numbers.sum))
      case Average(numbers)          => Right(Result(x, numbers.sum / numbers.length))
      case Min(numbers)              => Right(Result(x, numbers.min))
      case Max(numbers)              => Right(Result(x, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case Result(Divide(dividend, divisor), result) => s"$dividend divided by $divisor is $result"
      case Result(Sum(numbers), result)              => s"the sum of $numbers is $result"
      case Result(Average(numbers), result)          => s"the average of $numbers is $result"
      case Result(Min(numbers), result)              => s"the minimum of $numbers is $result"
      case Result(Max(numbers), result)              => s"the maximum of $numbers is $result"
    }
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result)
      .fold(_.value, renderResult)
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

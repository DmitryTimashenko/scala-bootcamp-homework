package com.evolutiongaming.bootcamp.basics

object AlgebraicDataTypes {

  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.
  final case class ErrorMessage(value: String)

  sealed trait Suit

  object Suit {

    final case object Diamond extends Suit

    final case object Spade extends Suit

    final case object Club extends Suit

    final case object Heart extends Suit

    def create(value: Char): Either[ErrorMessage, Suit] = {
      value match {
        case 'D'  => Right(Diamond)
        case 'S'  => Right(Spade)
        case 'C'  => Right(Club)
        case 'H'  => Right(Heart)
        case suit => Left(ErrorMessage(s"Error: Unable to parse suit $suit"))
      }
    }
  }

  sealed trait Rank

  object Rank {

    final case object Two extends Rank

    final case object Three extends Rank

    final case object Four extends Rank

    final case object Five extends Rank

    final case object Six extends Rank

    final case object Seven extends Rank

    final case object Eight extends Rank

    final case object Nine extends Rank

    final case object Ten extends Rank

    final case object Jack extends Rank

    final case object Queen extends Rank

    final case object King extends Rank

    final case object Ace extends Rank

    def create(value: Char): Either[ErrorMessage, Rank] = {
      value match {
        case '2'  => Right(Two)
        case '3'  => Right(Three)
        case '4'  => Right(Four)
        case '5'  => Right(Five)
        case '6'  => Right(Six)
        case '7'  => Right(Seven)
        case '8'  => Right(Eight)
        case '9'  => Right(Nine)
        case 'T'  => Right(Ten)
        case 'J'  => Right(Jack)
        case 'Q'  => Right(Queen)
        case 'K'  => Right(King)
        case 'A'  => Right(Ace)
        case rank => Left(ErrorMessage(s"Error: Unable to parse rank $rank"))
      }
    }
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Board

  object Board {

    final case class OmahaBoard(cards: List[Card]) extends Board

    final case class TexasBoard(cards: List[Card]) extends Board

    def createCreateOmahaBoard(cards: List[Card]): Either[ErrorMessage, OmahaBoard] = {
      if (cards.length != 4) Left(ErrorMessage("Error: Incorrect board number of cards"))
      else Right(OmahaBoard(cards))
    }

    def createCreateTexasBoard(cards: List[Card]): Either[ErrorMessage, TexasBoard] = {
      if (cards.length != 5) Left(ErrorMessage("Error: Incorrect board number of cards"))
      else Right(TexasBoard(cards))
    }
  }

  sealed trait PokerCombination

  object PokerCombination {

    case object HighCard extends PokerCombination

    case object OnePair extends PokerCombination

    case object TwoPair extends PokerCombination

    case object ThreeOfKind extends PokerCombination

    case object Straight extends PokerCombination

    case object Flush extends PokerCombination

    case object FullHouse extends PokerCombination

    case object FourOfKind extends PokerCombination

    case object StraightFlush extends PokerCombination

  }

  sealed trait Hand

  object Hand {

    final case class OmahaHand(cards: List[Card]) extends Hand

    final case class TexasHand(cards: List[Card]) extends Hand

    def createOmahaHand(cards: List[Card]): Either[ErrorMessage, OmahaHand] = {
      if (cards.length != 4) Left(ErrorMessage("Error: Incorrect hand number of cards"))
      else Right(new OmahaHand(cards))
    }

    def createTexasHand(cards: List[Card]): Either[ErrorMessage, OmahaHand] = {
      if (cards.length != 2) Left(ErrorMessage("Error: Incorrect hand number of cards"))
      else Right(new OmahaHand(cards))
    }
  }

  final case class TestCase(board: Board, hands: List[Hand])

  final case class TestResult(cases: List[TestCase])
}

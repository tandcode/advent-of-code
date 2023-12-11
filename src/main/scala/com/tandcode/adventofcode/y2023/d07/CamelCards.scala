package com.tandcode.adventofcode.y2023.d07

import com.tandcode.adventofcode.y2023.d07.CamelCards.Card
import com.tandcode.adventofcode.y2023.d07.CamelCards.Card._

import scala.io.Source
import scala.util.Using

object CamelCards extends App {

  enum Card(val value: String):
    case N2 extends Card("2")
    case N3 extends Card("3")
    case N4 extends Card("4")
    case N5 extends Card("5")
    case N6 extends Card("6")
    case N7 extends Card("7")
    case N8 extends Card("8")
    case N9 extends Card("9")
    case T extends Card("T")
    case J extends Card("J")
    case Q extends Card("Q")
    case K extends Card("K")
    case A extends Card("A")

  object Card:
    def apply(value: String): Card =
      values.find(_.value == value).getOrElse(throw new IllegalArgumentException(s"Cannot parse value: $value"))

    val defaultOrdering: Ordering[Card] = Ordering.by[Card, Int](_.ordinal)

    val jokerOrdering: Ordering[Card] = (c1: Card, c2: Card) => {
      if c1 == c2 then 0
      else c1 match
        case J => -1
        case _ => c2 match
          case J => 1
          case _ => c1.ordinal.compare(c2.ordinal)
    }
  end Card

  enum HandWeight:
    case HighCard, OnePair, TwoPair, ThreeOfKind, FullHouse, FourOfKind, FiveOfKind

  object HandWeight:
    def apply(cards: Seq[Card], withJoker: Boolean = false): HandWeight = {
      require(cards.size == 5, "expected 5 cards input")

      val cardToTotal = cards
        .groupBy(identity)
        .map { case (card, list) => card -> list.size }

      val jokers = if withJoker then cardToTotal.getOrElse(J, 0) else 0
      val cardToTotalWithoutJoker = if withJoker then cardToTotal - J else cardToTotal
      val occurrencesToCards = cardToTotalWithoutJoker
        .groupBy(_._2)
        .map { case (occurrences, cardToSize) => occurrences -> cardToSize.keySet.size }


      // returns number of occurrence and how much jokers are left
      def occurrenceToJokers(occurs: Int, jokers: Int): (Int, Int) = {
        if jokers == 5 then 1 -> 0
        else ((occurs - jokers) to occurs)
          .findLast(c => occurrencesToCards.contains(c))
          .map(c => occurrencesToCards(c) -> (jokers - (occurs - c)))
          .getOrElse(0 -> 0)
      }

      val (fives, _) = occurrenceToJokers(5, jokers)
      val (fours, _) = occurrenceToJokers(4, jokers)
      val (threes, leftAt3) = occurrenceToJokers(3, jokers)
      val (twosAfterThree, _) = occurrenceToJokers(2, leftAt3)
      val twosExcludeUsed = twosAfterThree - (jokers - leftAt3)
      val (twos, _) = occurrenceToJokers(2, 0)
      val twosWithJokers = twos + jokers

      if fives > 0 then FiveOfKind
      else if fours > 0 then FourOfKind
      else if threes > 0 && twosExcludeUsed > 0 then FullHouse
      else if threes > 0 then ThreeOfKind
      else if twosWithJokers > 1 then TwoPair
      else if twosWithJokers > 0 then OnePair
      else HighCard
    }
  end HandWeight

  case class Hand(cards: Seq[Card], value: Long) {
    private val weight: HandWeight = HandWeight(cards)
    private val jokerWeight: HandWeight = HandWeight(cards, true)
  }

  object Hand:
    val defaultOrdering: Ordering[Hand] = (hand1: Hand, hand2: Hand) => {
      val totalWeightComp = hand1.weight.ordinal.compare(hand2.weight.ordinal)

      if totalWeightComp != 0 then totalWeightComp
      else hand1.cards.zip(hand2.cards)
        .find { case (card, card1) => card != card1 }
        .map { case (card, card1) => card.ordinal.compare(card1.ordinal) }
        .getOrElse(0)
    }

    val jokerOrdering: Ordering[Hand] = (hand1: Hand, hand2: Hand) => {
      val totalWeightComp = hand1.jokerWeight.ordinal.compare(hand2.jokerWeight.ordinal)

      if totalWeightComp != 0 then totalWeightComp
      else {
        val hcards1 = hand1.cards.map(_.value).mkString
        val hcards2 = hand2.cards.map(_.value).mkString

        val i = hand1.cards.zip(hand2.cards)
          .find { case (card, card1) => card != card1 }
          .map { case (card, card1) => Card.jokerOrdering.compare(card, card1) }
          .getOrElse(0)

        val cond = hand1.jokerWeight == hand2.jokerWeight && hcards1.contains("J") || hcards2.contains("J")
        i
      }
    }

  end Hand

  def countWeightOfCards(input: String, withJoker: Boolean = false): Long = {
    val hands = input.split("[\\r\\n]+")
      .map(line => {
        val Array(cards, value) = line.split("\\s+")
        Hand(
          cards.map(c => Card(c.toString)),
          value.toLong
        )
      })

    hands
      .sorted(if withJoker then Hand.jokerOrdering else Hand.defaultOrdering)
      .zipWithIndex
      .foldLeft(0L) { case (prevValue, handToIndex) => prevValue + handToIndex._1.value * (handToIndex._2 + 1) }
  }

  val testValues =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  val inputValues: String = Using(Source.fromResource("y2023/d07/input.txt"))(_.mkString).getOrElse("")

  println(s"Test count weight of cards: ${countWeightOfCards(testValues)}")
  println(s"Input count weight of cards: ${countWeightOfCards(inputValues)}")

  println(s"Test count weight of cards with joker: ${countWeightOfCards(testValues, true)}")
  println(s"Input count weight of cards with joker: ${countWeightOfCards(inputValues, true)}")
}

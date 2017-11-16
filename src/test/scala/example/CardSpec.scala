package scalapoker

import org.scalatest._
import scala.collection.immutable.TreeSet
import scala.annotation.tailrec

sealed abstract class Suit(val name: String)
case object Hearts extends Suit("hearts")
case object Clubs extends Suit("clubs")
case object Spades extends Suit("spades")
case object Diamonds extends Suit("diamonds")


sealed abstract class Rank (
  val name: String,
  val value: Int,
  val code: Char) extends Ordered[Rank] {
    def compare(that: Rank) = this.value - that.value
  }

case object Deuce extends Rank("Deuce", 2, '2')
case object Three extends Rank("Three", 3, '3')
case object Four extends Rank("Four", 4, '4')
case object Five extends Rank("Five", 5, '5')
case object Six extends Rank("Six", 6, '6')
case object Seven extends Rank("Seven", 7, '7')
case object Eight extends Rank("Eight", 8, '8')
case object Nine extends Rank("Nine", 9, '9')
case object Ten extends Rank("Ten", 10, 'T')
case object Jack extends Rank("Jack", 11, 'J')
case object Queen extends Rank("Queen", 12, 'Q')
case object King extends Rank("King", 13, 'K')
case object Ace extends Rank("Ace", 14, 'A')

case class Card (
  val rank : Rank,
  val suit: Suit) extends Ordered[Card] {

  def compare(that: Card) = this.rank.compare(that.rank)
  def description() = this.rank.name + " of " + this.suit.name
}

sealed abstract class Hand (
  val order: Int ) extends Ordered[Hand] {

  def description () : String

  def compare(that: Hand) : Int = (this, that) match {

    case (HighestCard(a), HighestCard(b)) => a compare b
    case (Pair(a), Pair(b)) => a compare b
    case (ThreeOfAKind(a), ThreeOfAKind(b)) => a compare b
    case (FourOfAKind(a), FourOfAKind(b)) => a compare b
    case _ => return this.order - that.order

  }
}

object Hands {


  type decider = List[Card] => Option[Hand]

  def selectStraight(cards: List[Card]) : Option[Hand] = {
    @tailrec
    def testStraight(cards: List[Card]) : Boolean = cards match {
      case Nil => false
      case x::Nil => true
      case x::y::zs => if (y.rank.value - x.rank.value == 1) {
                          testStraight (y::zs) 
                       } else {
                         false
                       }
    }

    if (testStraight(cards)) {
      val card = cards.max
      Some(Straight(card.rank))
    } else {
      None
    }

  }

  def selectGroup(cards: List[Card]) : Option[Hand] = {
      val byRank = cards.groupBy(_.rank)
      val groups = byRank map { case (rank, vals) => (rank, vals.length) }
      groups maxBy { case (rank, size) => size } match {
        case (r: Rank, 2) => Some(Pair(r))
        case (r: Rank, 3) => Some(ThreeOfAKind(r))
        case (r: Rank, 4) => Some(FourOfAKind(r))
        case _ => None
    }
  }

  @tailrec
  def firstmatch(funcs: List[decider], cards: List[Card]) : Option[Hand] = funcs match {
    case Nil => None
    case f :: rest => (f(cards)) match {
      case None => firstmatch(rest, cards)
      case h => h
    }
  }

  def select(cards: List[Card]) : Hand =  {
    val deciders = List(selectStraight _, selectGroup _)
    firstmatch(deciders, cards) match {
      case Some(hand) => hand
      case None => HighestCard(cards.max)
    }
  }
}


case class HighestCard(card: Card) extends Hand (0) {
  def description() = "High card: " + card.description()
}

case class Pair(rank: Rank) extends Hand(1) {
  def description() = "Pair of " + rank.name + "s"
}

case class ThreeOfAKind(rank: Rank) extends Hand(2) {
  def description() = "Three of a kind: " + rank.name
}

case class FourOfAKind(rank: Rank) extends Hand(3) {
  def description() = "Four of a kind: " + rank.name
}

case class Straight(rank: Rank) extends Hand(4) {
  def description() = "Straight: " + rank.name + " high"
}

class CardSpec extends FlatSpec with Matchers {

  behavior of "Two cards"

  it should "be orderable by rank" in {
    val fst = Card(Deuce, Spades)
    val snd = Card(Ace, Spades)

    assert (snd > fst)
    assert (fst < snd)
  }
}

class SameRankSpec extends FlatSpec with Matchers {

  behavior of "Two cards of the same rank"

  it should "be neither greater nor smaller than each other" in {
    val fst = Card(Deuce, Spades)
    val snd = Card(Deuce, Hearts)

    assert (! (fst > snd))
    assert (! (fst < snd))
  }

  it should "not be equal" in {
    val fst = Card(Deuce, Spades)
    val snd = Card(Deuce, Hearts)
    assert (fst != snd)
  }
}


class HighestCardSpec extends FlatSpec with Matchers {

  behavior of "Highest card"

  it should "be ordered by the value of the card" in {

    val fst = HighestCard(Card(Deuce, Hearts))
    val snd = HighestCard(Card(Three, Hearts))

    assert (fst < snd)
    assert (snd > fst)
  }

  it should "have the right description" in {
    val hand = HighestCard(Card(Ace, Spades))
    assert (hand.description()  === "High card: Ace of spades")
  }

}


class PairSpec extends FlatSpec with Matchers {
  behavior of "Pair"

  it should "beat a highest card" in {
    val pair: Hand = Pair(Deuce)
    val highest : Hand = HighestCard(Card(King, Clubs))

    pair should be > highest
  }

  it should "have the correct description" in {
    val pair = Pair(Ten)
    pair.description should equal ("Pair of Tens")
  }

  it should "be ordered by rank" in {
    val king : Hand = Pair(King)
    val queen : Hand = Pair(Queen)

    king should be > queen
  }
}


class ThreeOfAKindSpec extends FlatSpec with Matchers {
  behavior of "Three of a kind"

  it should "beat a pair" in {
    val pair: Hand = Pair(King)
    val threeOfAKind : Hand = ThreeOfAKind(Deuce)

    threeOfAKind should be > pair
  }

  it should "have the correct description" in {
    val threeOfAKind = ThreeOfAKind(Ten)
    threeOfAKind.description should equal ("Three of a kind: Ten")
  }

  it should "be ordered by rank" in {
    val four : Hand = ThreeOfAKind(Four)
    val ace : Hand = ThreeOfAKind(Ace)

    ace should be > four
  }
}


class FourOfAKindSpec extends FlatSpec with Matchers {
  behavior of "Four of a kind"

  it should "beat three of a kind" in {
    val threeOfAKind : Hand = ThreeOfAKind(Deuce)
    val fourOfAKind : Hand = FourOfAKind(Deuce)

    fourOfAKind should be > threeOfAKind
  }

  it should "have the correct description" in {
    val fourOfAKind = FourOfAKind(King)
    fourOfAKind.description should equal ("Four of a kind: King")
  }

  it should "be ordered by rank" in {
    val four : Hand = FourOfAKind(Four)
    val ace : Hand = FourOfAKind(Ace)

    ace should be > four
  }
}

class HandFactorySpec extends FlatSpec with Matchers {

  behavior of "Hand Factory"

  it should "select the highest card in the hand for Highest Card" in {
    val cards = List(
      Card(Nine, Clubs),
      Card(King, Diamonds),
      Card(Seven, Hearts))

    val hand = Hands.select(cards)

    hand shouldBe a [HighestCard]

    val hc = hand.asInstanceOf[HighestCard]
    hc.card should equal (Card(King, Diamonds))
  }

  it should "return a pair of matching cards by rank" in {
    val cards = List(
      Card(Four, Hearts),
      Card(King, Diamonds),
      Card(Four, Clubs))

    val hand = Hands.select(cards)

    hand shouldBe a [Pair]

    val pair = hand.asInstanceOf[Pair]
    pair.rank should equal(Four)
  }


  it should "return three of a kind by rank" in {
    val cards = List(
      Card(Four, Hearts),
      Card(Four, Diamonds),
      Card(Four, Clubs))

    val hand = Hands.select(cards)

    hand shouldBe a [ThreeOfAKind]

    val triplet = hand.asInstanceOf[ThreeOfAKind]
    triplet.rank should equal(Four)
  }


  it should "return four of a kind by rank" in {
    val cards = List(
      Card(Six, Hearts),
      Card(Six, Diamonds),
      Card(Six, Spades),
      Card(Six, Clubs))

    val hand = Hands.select(cards)

    hand shouldBe a [FourOfAKind]

    val quadruplet = hand.asInstanceOf[FourOfAKind]
    quadruplet.rank should equal(Six)
  }

  it should "return straights" in {
    val cards = List(
      Card(Deuce, Hearts),
      Card(Three, Diamonds),
      Card(Four, Spades),
      Card(Five, Clubs),
      Card(Six, Clubs))

    val hand = Hands.select(cards)
    hand shouldBe a [Straight]
  }

}

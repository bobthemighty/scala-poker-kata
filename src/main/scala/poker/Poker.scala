package scalapoker
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
    case (Straight(a), Straight(b)) => a compare b
    case (Flush(a), Flush(b)) => a compare b
    case (FullHouse(_, thisTriple), FullHouse(_, thatTriple)) => thisTriple compare thatTriple
    case (StraightFlush(a), StraightFlush(b)) => a compare b
    case _ => return this.order - that.order

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

case class Straight(rank: Rank) extends Hand(3) {
  def description() = "Straight: " + rank.name + " high"
}

case class Flush(card: Card) extends Hand(4) {
  def description() = "Flush of " + card.suit.name + ", "+ card.rank.name +" high"
}

case class FullHouse(pair: Rank, triplet: Rank) extends Hand(5) {
  def description() = "Full house: " + triplet.name + " over "+ pair.name
}

case class FourOfAKind(rank: Rank) extends Hand(6) {
  def description() = "Four of a kind: " + rank.name
}

case class StraightFlush(rank: Rank) extends Hand(7) {
  def description() = "Straight flush: " + rank.name + " high"
}


object Hands {

  type decider = List[Card] => Option[Hand]

  def isStraight(cards: List[Card]) : Boolean = {
    val ranks = cards.map(_.rank)
    ranks.sliding(2).forall(pair => pair(0).value + 1 == pair(1).value)
  }

  def haveSameSuit(cards: List[Card]) : Boolean = {
    cards.forall(c => c.suit == cards(0).suit)
  }

  def countByRank(cards: List[Card]) = {
    val byRank = cards.groupBy(_.rank)
    byRank map { case (rank, vals) => (rank, vals.length) }
  }


  def selectFullHouse(cards: List[Card]) : Option[Hand] ={
    val groups = countByRank(cards)
    (
      groups.find{ case (r, s) => s == 3 },
      groups.find{ case (r, s) => s == 2 },
    ) match {
      case (Some((triplet,_)), Some((pair,_))) => Some(FullHouse(pair, triplet))
      case _ => None
    }
  }

  def selectFlush(cards: List[Card]): Option[Hand] = {
    if (haveSameSuit (cards)) Some(Flush(cards.max)) else None
  }

  def selectStraightFlush(cards: List[Card]) : Option[Hand] = {
    if (isStraight(cards) && haveSameSuit(cards)) {
      Some(StraightFlush(cards.max.rank)) 
    } else {
      None
    }
  }

  def selectStraight(cards: List[Card]) : Option[Hand] = {
    if (isStraight(cards)) Some(Straight(cards.max.rank)) else None
  }

  def selectGroup(cards: List[Card]): Option[Hand] = {
    val groups = countByRank(cards)
    groups maxBy { case (rank, size) => size } match {
      case (r: Rank, 2) => Some(Pair(r))
      case (r: Rank, 3) => Some(ThreeOfAKind(r))
      case (r: Rank, 4) => Some(FourOfAKind(r))
      case _ => None
    }
  }

  @tailrec
  def firstmatch(funcs: List[decider], cards:List[Card]) : Option[Hand] = funcs match {
    case Nil => None
    case f :: rest => (f(cards)) match {
      case None => firstmatch(rest, cards)
      case h => h
    }
  }

  def select(cards: List[Card]) : Hand =  {

    val ordered = cards.sorted

    val deciders = List(
      selectStraightFlush _,
      selectFullHouse _,
      selectFlush _,
      selectStraight _, 
      selectGroup _
    )
    firstmatch(deciders, ordered).getOrElse(HighestCard(cards.max)) 
  }
}



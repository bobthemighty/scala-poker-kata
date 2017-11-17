package scalapoker

import org.scalatest._
import scala.collection.immutable.TreeSet

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

class TwoPairsSpec extends FlatSpec with Matchers {
  behavior of "Two pairs"

  it should "beat a pair" in {
    val pair: Hand = Pair(Ace)
    val twopairs : Hand = TwoPairs(Deuce, Three)

    twopairs should be > pair
  }

  it should "have the correct description" in {
    val pair = TwoPairs(Three, Ten)
    pair.description should equal ("Pair of Threes and pair of Tens")
  }

  it should "be ordered by rank" in {
    val king : Hand = TwoPairs(Deuce, King)
    val queen : Hand = TwoPairs(Three, Queen)

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

  it should "beat a full house" in {
    val fullhouse : Hand = FullHouse(Deuce, Three)
    val fourOfAKind : Hand = FourOfAKind(Deuce)

    fourOfAKind should be > fullhouse
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


class StraightSpec extends FlatSpec with Matchers {
  behavior of "Straight"

  it should "beat three of a kind" in {
    val threeOfAKind : Hand = ThreeOfAKind(King)
    val straight : Hand = Straight(Seven)

    straight should be > threeOfAKind
  }

  it should "have the correct description" in {
    val straight : Hand = Straight(Seven)
    straight.description should equal ("Straight: Seven high")
  }

  it should "order by rank" in {
    val seven : Hand = Straight(Seven)
    val eight : Hand = Straight(Eight)

    seven should be < eight
  }
}

class FlushSpec extends FlatSpec with Matchers {
  behavior of "Flush"

  it should "beat a straight" in {
    val straight : Hand = Straight(Seven)
    val flush : Hand = Flush(Card(Deuce, Clubs))

    flush should be > straight
  }

  it should "have the correct description" in {
    val flush : Hand = Flush(Card(Seven, Diamonds))
    flush.description should equal ("Flush of diamonds, Seven high")
  }

  it should "order by rank" in {
    val seven : Hand = Flush(Card(Seven, Clubs))
    val eight : Hand = Flush(Card(Eight, Clubs))

    seven should be < eight
  }
}

class FullHouseSpec extends FlatSpec with Matchers {
  behavior of "Full House"

  it should "beat a flush" in {
    val flush : Hand = Flush(Card(Deuce, Clubs))
    val fullhouse : Hand = FullHouse(Deuce, Seven)

    fullhouse should be > flush
  }

  it should "have the correct description" in {
    val fullhouse : Hand = FullHouse(Ace, King)
    fullhouse.description should equal ("Full house: King over Ace")
  }

  it should "order by the rank of the triplet" in {
    val seven : Hand = FullHouse(Deuce, Seven)
    val eight : Hand = FullHouse(Ace, Eight)

    seven should be < eight
  }
}

class StraightFlushSpec extends FlatSpec with Matchers {
  behavior of "A straight flush"

  it should "beat four of a kind" in {
    val fourofakind : Hand = FourOfAKind(Ace)
    val straightflush : Hand = StraightFlush(Deuce)

    straightflush should be > fourofakind
  }

  it should "have the correct description" in {
    val fullhouse : Hand = StraightFlush(Deuce)
    fullhouse.description should equal ("Straight flush: Deuce high")
  }

  it should "order by rank" in {
    val seven : Hand = StraightFlush(Seven)
    val eight : Hand = StraightFlush(Eight)

    seven should be < eight
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

  it should "return two pairs of matching cards by rank" in {
    val cards = List(
      Card(Four, Hearts),
      Card(King, Diamonds),
      Card(King, Diamonds),
      Card(Four, Clubs))

    val hand = Hands.select(cards)

    hand shouldBe a [TwoPairs]

    val pair = hand.asInstanceOf[TwoPairs]
    pair.high should equal(King)
    pair.low should equal(Four)
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

    val straight = hand.asInstanceOf[Straight]
    straight.rank should equal(Six)
  }

  it should "return a flush when all cards have the same suit" in {
    val cards = List(
      Card(Deuce, Hearts),
      Card(Ten, Hearts),
      Card(Four, Hearts),
      Card(Nine, Hearts),
      Card(Queen, Hearts))

    val hand = Hands.select(cards)
    hand shouldBe a [Flush]

    val flush = hand.asInstanceOf[Flush]
    flush.card should equal(Card(Queen, Hearts))
  }

  it should "select a full house when given a pair and three of a kind" in {
    val cards = List(
      Card(Deuce, Hearts),
      Card(Deuce, Clubs),
      Card(Four, Hearts),
      Card(Four, Diamonds),
      Card(Four, Spades))

    val hand = Hands.select(cards)
    hand shouldBe a [FullHouse]

    val fullhouse = hand.asInstanceOf[FullHouse]
    fullhouse.triplet should equal(Four)
    fullhouse.pair should equal(Deuce)
  }

  it should "return a straight flush for consecutive cards of the same suit" in {
    val cards = List(
      Card(Deuce, Hearts),
      Card(Three, Hearts),
      Card(Four, Hearts),
      Card(Five, Hearts),
      Card(Six, Hearts)
    )

    val hand = Hands.select(cards)
    hand shouldBe a [StraightFlush]

    val straight = hand.asInstanceOf[StraightFlush]
    straight.rank should equal (Six)
  }
}

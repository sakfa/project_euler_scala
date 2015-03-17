package pl.sakfa.project_euler.solutions

sealed trait Suit
case object Hearts extends Suit
case object Clubs extends Suit
case object Spades extends Suit
case object Diamonds extends Suit

case class Card(value: Int, suit: Suit)
object Card {
    def fromString(s: String): Card = {
        val value = s(0) match {
            case 'T' => 10
            case 'J' => 11
            case 'Q' => 12
            case 'K' => 13
            case 'A' => 14
            case c: Char => c.toInt - '0'.toInt
        }
        val suit = s(1) match {
            case 'H' => Hearts
            case 'C' => Clubs
            case 'S' => Spades
            case 'D' => Diamonds
        }
        Card(value, suit)
    }
}

trait Hand extends Ordered[Hand] {
    val cards: IndexedSeq[Card]
    val rank: Int

    val power: Int
    val base = 15;
    val size = 5;
    val rankPowerBase = math.pow(base, size).toInt;
    val rankBase = math.pow(base, size+2).toInt;

    def weightedPower(xs: IndexedSeq[Card]): Int = {
        xs.zipWithIndex.map {
            case(card, i) => card.value * ( math.pow(base, (size-i-1)).toInt )
        }.sum
    }
    def compare(that: Hand) = (this.rank*rankBase + this.power) - (that.rank*rankBase + that.power);
}

object RoyalFlush {
    def unapply(xs: IndexedSeq[Card]) = if (xs.head.value == 14 && !StraightFlush.unapply(xs).isEmpty) Some(RoyalFlush(xs)) else None;
}
case class RoyalFlush(cards: IndexedSeq[Card]) extends Hand {
    val rank = 9;
    val power = 1;
}
object StraightFlush {
    def unapply(xs: IndexedSeq[Card]) = if (!Straight.unapply(xs).isEmpty && !Flush.unapply(xs).isEmpty) Some(StraightFlush(xs)) else None;
}
case class StraightFlush(cards: IndexedSeq[Card]) extends Hand {
    val rank = 8;
    val power = weightedPower(cards)
}
object FourOfAKind {
    def unapply(xs: IndexedSeq[Card]) = if (xs(0).value == xs(3).value || xs(1).value == xs(4).value) Some(FourOfAKind(xs)) else None;
}
case class FourOfAKind(cards: IndexedSeq[Card]) extends Hand {
    val rank = 7;
    val power = {
        cards(2).value * rankPowerBase + weightedPower(cards)
    }
}
object FullHouse {
    def unapply(xs: IndexedSeq[Card]) = if (
                      (xs(0).value == xs(2).value && xs(3).value == xs(4).value) ||
                      (xs(0).value == xs(1).value && xs(2).value == xs(4).value))
                Some(FullHouse(xs)) else None;
}
case class FullHouse(cards: IndexedSeq[Card]) extends Hand {
    val rank = 6;
    val power = {
        val (three, pair) = if (cards(0).value == cards(2).value && cards(3).value == cards(4).value)
            (cards(0).value, cards(3).value) else (cards(2).value, cards(0).value);
        (15*three + pair) * rankPowerBase + weightedPower(cards);
    }
}
object Flush {
    def unapply(xs: IndexedSeq[Card]) = if (xs.forall(c => c.suit == xs(0).suit)) Some(Flush(xs)) else None;
}
case class Flush(cards: IndexedSeq[Card]) extends Hand {
    val rank = 5;
    val power = weightedPower(cards)
}
object Straight {
    def unapply(xs: IndexedSeq[Card]) = if (xs.sliding(2).forall(cs => cs.head.value == cs.tail.head.value + 1)) Some(Straight(xs)) else None;
}
case class Straight(cards: IndexedSeq[Card]) extends Hand {
    val rank = 5;
    val power = weightedPower(cards)
}
object ThreeOfAKind {
    def unapply(xs: IndexedSeq[Card]) = if ((0 to 2).exists(n => xs(n).value == xs(n+2).value)) Some(ThreeOfAKind(xs)) else None;
}
case class ThreeOfAKind(cards: IndexedSeq[Card]) extends Hand {
    val rank = 4;
    val power = {
        cards(2).value * rankPowerBase + weightedPower(cards)
    }
}
object TwoPairs {
    def unapply(xs: IndexedSeq[Card]) = {
        if (xs.sliding(2).filter(cs => cs.head.value == cs.tail.head.value).size == 2) Some(TwoPairs(xs)) else None
    }
}
case class TwoPairs(cards: IndexedSeq[Card]) extends Hand {
    val rank = 3;
    val power = {
        val pairValues = cards.sliding(2).filter(cs => cs.head.value == cs.tail.head.value).map(_.head.value).toStream
        val pairsPower = 15*pairValues.head + pairValues.tail.head;
        pairsPower * rankPowerBase + weightedPower(cards);
    }
}
object OnePair {
    def unapply(xs: IndexedSeq[Card]) = if (xs.sliding(2).exists(cs => cs.head.value == cs.tail.head.value)) Some(OnePair(xs)) else None
}
case class OnePair(cards: IndexedSeq[Card]) extends Hand {
    val rank = 2;
    val power = {
        val pairValue = cards.sliding(2).filter(cs => cs.head.value == cs.tail.head.value).map(_.head.value).toStream.head
        pairValue * rankPowerBase + weightedPower(cards)
    }
}
object HighCard {
    def unapply(xs: IndexedSeq[Card]) = Some(HighCard(xs))
}
case class HighCard(cards: IndexedSeq[Card]) extends Hand {
    val rank = 1
    val power = weightedPower(cards);
}
object Hand {
    def fromUnorderedCards(cards: IndexedSeq[Card]): Hand = {
        cards.sortBy(_.value).reverse match {
            case RoyalFlush(hand) => hand
            case StraightFlush(hand) => hand
            case FourOfAKind(hand) => hand
            case FullHouse(hand) => hand
            case Flush(hand) => hand
            case Straight(hand) => hand
            case ThreeOfAKind(hand) => hand
            case TwoPairs(hand) => hand
            case OnePair(hand) => hand
            case HighCard(hand) => hand
        }
    }
}

case class Duel(hand1: Hand, hand2: Hand) {
    def player1Wins = hand1 > hand2;
}

object Problem54 extends App {
    val games = {
        io.Source.fromInputStream(getClass.getResourceAsStream("/problem54_poker.txt")).getLines
            .map(line => {
                val cards = line.split(" ").map(s => Card.fromString(s))
                Duel(Hand.fromUnorderedCards(cards.slice(0,5)),
                     Hand.fromUnorderedCards(cards.slice(5,10)) )
            })
    }

    println(games.filter(_.player1Wins).size);
}
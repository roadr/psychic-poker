package ijsenese.hyvestest

import Face._
import Seed._

/*
* Ranks available in the game of psychic poker
* */
object Rank extends Enumeration {
    type PokerRank = Value

    val HighestCard = Value("highest-card")
    val OnePair = Value("one-pair")
    val TwoPairs = Value("two-pairs")
    val ThreeOfAKind = Value("three-of-a-kind")
    val Straight = Value("straight")
    val Flush = Value("flush")
    val FullHouse = Value("full-house")
    val FourOfAKind = Value("four-of-a-kind")
    val StraightFlush = Value("straight-flush")
}

/*
* Order existing between the ranks
* */
object RankOrdering extends Ordering[Rank.PokerRank] {
    import Rank._
    def rankOrder = List(HighestCard, OnePair, TwoPairs, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush)
    def compare(x: Rank.PokerRank, y: Rank.PokerRank): Int = {
        rankOrder.indexOf(x) - rankOrder.indexOf(y)
    }
}

/*
* Factory for getting the rank for a list of 5 cards
* */
object HandRank {

    import Rank._

    def apply(cards: List[Card]) = {
        require(cards.length == 5)
        cards match {
            case _ if isStraightFlush(cards) => StraightFlush
            case _ if isFourOfAKind(cards) => FourOfAKind
            case _ if isFullHouse(cards) => FullHouse
            case _ if containsFlush(cards) => Flush
            case _ if containsStraight(cards) => Straight
            case _ if containsThreeOfAKind(cards) => ThreeOfAKind
            case _ if containsTwoPairs(cards) => TwoPairs
            case _ if containsPair(cards) => OnePair
            case _ => HighestCard
        }
    }

    private def isStraightFlush(cards: List[Card]) = {
        containsFlush(cards) && containsStraight(cards)
    }

    private def isFourOfAKind(cards: List[Card]) = {
        val frequencies = frequency[FaceValue](cards map (_.faceValue))
        frequencies exists (_._2 == 4)
    }

    private def isFullHouse(cards: List[Card]) = {
        val frequencies = frequency[FaceValue](cards map (_.faceValue))
        (frequencies.keys.size == 2) && (frequencies exists(_._2 ==2))
    }

    private def containsFlush(cards: List[Card]) = {
        val frequencies = frequency[SeedValue](cards map (_.seedValue))
        frequencies.keys.size == 1
    }

    private def containsStraight(cards: List[Card]) = {
        possibleStraights exists (_ -- (cards.map(card => card.faceValue).toSet) isEmpty)
    }

    private def containsThreeOfAKind(cards: List[Card]) = {
        val frequencies = frequency[FaceValue](cards map (_.faceValue))
        frequencies.exists(_._2 > 2)
    }

    private def containsTwoPairs(cards: List[Card]) = {
        val frequencies = frequency[FaceValue](cards map (_.faceValue))
        val (pairs, _) = frequencies partition {
            case (value, freq) if freq > 1 => true
            case _ => false
        }
        pairs.size > 1
    }

    private def containsPair(cards: List[Card]) = {
        val frequencies = frequency[FaceValue](cards map (_.faceValue))
        frequencies.exists(_._2 > 1)
    }

    private def frequency[T](values: List[T]) = {
        var frequencies = Map[T, Int]()
        values.foreach(value => frequencies.get(value) match {
            case Some(x) => frequencies += (value -> (x+1))
            case None => frequencies += (value -> 1)
        })
        frequencies
    }

//    private def frequencyByValue(cards: List[Card]) = {
//        var frequencies = Map[FaceValue, Int]()
//        cards.foreach(card => frequencies.get(card.value) match {
//            case Some(x) => frequencies += (card.value -> (x+1))
//            case None => frequencies += (card.value -> 1)
//        })
//        frequencies
//    }

//    private def frequencyBySeed(cards: List[Card]) = {
//        var frequencies = Map[SeedValue, Int]()
//        cards.foreach(card => frequencies.get(card.seed) match {
//            case Some(x) => frequencies += (card.seed -> (x+1))
//            case None => frequencies += (card.seed -> 1)
//        })
//        frequencies
//    }

    private def possibleStraights: List[Set[FaceValue]] = {
        List(
            Set(Ace, Two, Three, Four, Five),
            Set(Two, Three, Four, Five, Six),
            Set(Three, Four, Five, Six, Seven),
            Set(Four, Five, Six, Seven, Eight),
            Set(Five, Six, Seven, Eight, Nine),
            Set(Six, Seven, Eight, Nine, Ten),
            Set(Seven, Eight, Nine, Ten, Jack),
            Set(Eight, Nine, Ten, Jack, Queen),
            Set(Nine, Ten, Jack, Queen, King),
            Set(Ten, Jack, Queen, King, Ace)
        )
    }
}

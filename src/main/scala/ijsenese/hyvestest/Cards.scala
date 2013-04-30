package ijsenese.hyvestest

/*
* Seeds available in the game of psychic poker
* */
object Seed extends Enumeration {
    type SeedValue = Value
    val Hearts = Value("H")
    val Clubs = Value("C")
    val Spades = Value("S")
    val Diamonds = Value("D")
}

/*
* Face values available in the game of psychic poker
* */
object Face extends Enumeration {
    type FaceValue = Value
    val Ace = Value("A")
    val Two = Value("2")
    val Three = Value("3")
    val Four = Value("4")
    val Five = Value("5")
    val Six = Value("6")
    val Seven = Value("7")
    val Eight = Value("8")
    val Nine = Value("9")
    val Ten = Value("T")
    val Jack = Value("J")
    val Queen = Value("Q")
    val King = Value("K")
}

import Face._
import Seed._

case class Card(seed: SeedValue, value: FaceValue) {
    override def toString = {
        value.toString + seed.toString
    }
}

/*
* Factory for obtaining a Card instance given it's string representation
* */
object CardBuilder {
    def apply(cardString: String) = {
        require(cardString.length == 2)
        Card(Seed.withName(cardString.last.toString), Face.withName(cardString.head.toString))
    }
}
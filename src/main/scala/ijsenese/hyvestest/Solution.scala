package ijsenese.hyvestest

import scala.io.Source

object Solution extends App {

    //requires file name containing input data as argument
    require(args.length == 1, "Application requires input file as the only argument")

    val fileName = args(0)

    val inputStrings: List[List[String]] = readInputFile(fileName)

    //iterate over the strings contained in the file to get the best hand for each case
    for(inputString <- inputStrings) {
        val (handCards, deckCards) = parseCards(inputString)
        val rank = PsychicPokerPlayer.getBestHand(handCards, deckCards)
        prettyPrinter(handCards, deckCards, rank)
    }

    def readInputFile(fileName: String) = {
        Source.fromFile(fileName).getLines().map(_.split(" ").toList).toList
    }

    def parseCards(cards: List[String]) = {
        require(cards.distinct.size == 10)

        val handCards = cards.take(5) map (CardBuilder(_))
        val deckCards = cards.takeRight(5) map (CardBuilder(_))

        (handCards, deckCards)
    }

    def prettyPrinter(handCards: List[Card], deckCards: List[Card], rank: Rank.PokerRank) {
        println(
            "Hand: %s Deck: %s Best hand: %s"
                .format(handCards.take(5).mkString(" "), deckCards.takeRight(5).mkString(" "), rank)
        )
    }
}
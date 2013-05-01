package ijsenese.hyvestest

object PsychicPokerPlayer {

    /*
    * Returns the rank of the poker hand one can get given the starting hand and the cards on the deck
    * */
    def getBestHand(handCards: List[Card], deckCards: List[Card]) = {
        val ranks = for {
            cardsToTakeFromDeck <- 0 to 5
            cardsFromDeck = deckCards.take(cardsToTakeFromDeck)
            cardsPickedFromStartingHand <- handCards.combinations(5 - cardsToTakeFromDeck)
        } yield {
            HandRank(cardsPickedFromStartingHand ::: cardsFromDeck)
        }

        ranks max RankOrdering
    }
}


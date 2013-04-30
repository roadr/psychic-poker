package ijsenese.hyvestest;

import junit.framework._;
import Assert._;

import Face._
import Seed._
import Rank._

object PsychicPokerPlayerTest {
    def suite: Test = {
        val suite = new TestSuite(classOf[PsychicPokerPlayerTest]);
        suite
    }

    def main(args : Array[String]) {
        junit.textui.TestRunner.run(suite);
    }
}

/**
 * Unit test for simple App.
 */
class PsychicPokerPlayerTest extends TestCase("app") {

    def testTwoPairs = {

        val startingHand = List(
            Card(Seed.Hearts, Face.Ace),
            Card(Seed.Clubs, Face.Two),
            Card(Seed.Diamonds, Face.Nine),
            Card(Seed.Diamonds, Face.Ace),
            Card(Seed.Clubs, Face.Three)
        )
        val startingDeck = List(
            Card(Seed.Hearts, Face.Queen),
            Card(Seed.Spades, Face.King),
            Card(Seed.Spades, Face.Jack),
            Card(Seed.Diamonds, Face.Jack),
            Card(Seed.Diamonds, Face.King)
        )
        val rank = PsychicPokerPlayer.getBestHand(startingHand, startingDeck)
        assertEquals(rank, TwoPairs)
    }

    def testStraightFlush = {
        val startingHand = List(
            Card(Seed.Hearts, Face.Ten),
            Card(Seed.Hearts, Face.Jack),
            Card(Seed.Clubs, Face.Queen),
            Card(Seed.Diamonds, Face.Queen),
            Card(Seed.Spades, Face.Queen)
        )
        val startingDeck = List(
            Card(Seed.Hearts, Face.Queen),
            Card(Seed.Hearts, Face.King),
            Card(Seed.Hearts, Face.Ace),
            Card(Seed.Spades, Face.Two),
            Card(Seed.Spades, Face.Six)
        )
        val rank = PsychicPokerPlayer.getBestHand(startingHand, startingDeck)
        assertEquals(rank, StraightFlush)
    }
}

//   _____      _             _       _   _______       _ _ _       _     _
//  / ____|    | |           (_)     | | |__   __|     (_) (_)     | |   | |
// | |     ___ | | ___  _ __  _  __ _| |    | |_      ___| |_  __ _| |__ | |_
// | |    / _ \| |/ _ \| '_ \| |/ _` | |    | \ \ /\ / / | | |/ _` | '_ \| __|
// | |___| (_) | | (_) | | | | | (_| | |    | |\ V  V /| | | | (_| | | | | |_
//  \_____\___/|_|\___/|_| |_|_|\__,_|_|    |_| \_/\_/ |_|_|_|\__, |_| |_|\__|
//                                                             __/ |
//                                                            |___/
// A scala implementation of the solo AI for the game
// Colonial Twilight, designed by Brian Train and
// Published by GMT Games
//
// Copyright (c) 2017 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package coltwi.cards

import coltwi.ColonialTwilight._
import Card._
import coltwi.{ Human, Bot }

object Card_066 extends Card(66, "Coup d'etat", Single, false, false)
{
  override def botEventSelection(): EventSelection = NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // May play once per Campaign; retain this card. See 5.1.5.
    // Each player rolls 1d6.
    //
    // If Government rolls higher:
    // reCall De gaulle and/or OAS Events cancelled if in play;
    // add Commitment = lower of the 2 die rolls; add Resources = sum of the 2 rolls.
    //
    // If FLN rolls higher: choice of:
    // remove Troops = lower of the 2 die rolls from map or Available to Out of Play
    // (FLN may choose, no change in Commitment);
    // OR
    // subtract Commitment = lower of the 2 die rolls
    //
    //  If tied, no change (but card has been played).

    val govDie = dieRoll
    val flnDie = dieRoll
    log(s"Government die roll: $govDie")
    log(s"FLN die roll       : $flnDie")

    if (govDie == flnDie)
      log("The event has no effect")
    else if (govDie > flnDie) {
      // If Gov wins roll cancel Recall De Gaulle
      // Would also cancel/prevent OAS, but Bot never plays it...
      if (game.recallDeGaulleInEffect) {
        game = game.copy(recallDeGaulleCancelled = true)
        log("The Recall De Gaulle pivotal event is cancelled")
      }
      increaseCommitment(govDie min flnDie)
      increaseResources(Gov, govDie + flnDie)
    }
    else
      decreaseCommitment(govDie min flnDie)

  }
  override def executeShaded(role: Role): Unit = () // Single event
}

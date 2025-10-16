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

object Card_036 extends Card(36, "Assassination", Dual, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
    if (game.outOfPlay.hiddenGuerrillas > 0 || game.guerrillasAvailable > 0) Shaded else NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Crippling leadership loss: Remove 1 Guerrilla from
    // any space to casualties, -1d6 FLN resources
    if (game.totalOnMap(_.totalGuerrillas) == 0 && game.resources(Fln) == 0)
      log("The event has no effect")
    else {
      val candidates = spaceCandidates(_.totalGuerrillas > 0)
      if (candidates.nonEmpty) {
        val name      = askSpace("\nSelect space to remove guerrilla: ", candidates)
        val sp        = game.getSpace(name)
        val guerrilla = askPieces(sp.pieces, 1, GUERRILLAS)
        removeToCasualties(name, guerrilla)
      }
      decreaseResources(Fln, dieRoll)
    }

  }

  override def executeShaded(role: Role): Unit = {
    // Martyr: Add 1 Guerrilla in any space from
    // available or out of play, +1d6 FLN resources
    val priorities = List(
      new Bot.BooleanPriority[Space]("Support space",    _.isSupport),
        new Bot.BooleanPriority[Space]("Unprotected base", sp => sp.flnBases > 0 && (sp.hiddenGuerrillas == 0 || sp.totalGuerrillas < 2)),
      new Bot.BooleanPriority[Space]("Friendly pieces",  _.totalFln > 0),
      new Bot.BooleanPriority[Space]("In Algeria",      !_.isCountry),
      new Bot.HighestPriority[Space]("Highest population", _.population))
    val sp = Bot.topPriority(game.spaces, priorities)
    if (game.outOfPlay.hiddenGuerrillas > 0)
      placePiecesFromOutOfPlay(sp.name, Pieces(hiddenGuerrillas = 1))
    else
      placePieces(sp.name, Pieces(hiddenGuerrillas = 1))

    increaseResources(Fln, dieRoll)
  }
}

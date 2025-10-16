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

object Card_055 extends Card(55, "Development", Dual, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
      if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Infrastructure: Place up to 2 government bases anywhere in
    // Algeria from Available or Out of Play.
    val mostOutOfPlay = 2 min game.outOfPlay.govBases
    val fromOutOfPlay = if (mostOutOfPlay == 0) 0
                        else askInt("Place how many bases from Out of Play", 0, mostOutOfPlay)
    val mostInPlay  = (2 - fromOutOfPlay) min (game.govBasesAvailable + game.totalOnMap(_.govBases))
    val fromInPlay = if (fromOutOfPlay == 2) 0
                      else askInt("Place how many bases from available", 0, mostInPlay)

    def placeBases(remaining: Int, oop: Boolean): Unit = if (remaining > 0) {
      val prompt = s"\nSelect space for base from ${if (oop) "Out of Play" else "Available"}: "
      val name   = askSpace(prompt, algerianCandidates(_.canTakeBase))
      if (oop)
        placePiecesFromOutOfPlay(name, Pieces(govBases = 1))
      else {
        if (game.govBasesAvailable == 0)
          voluntaryRemoval(1, GovBases, Set(name))
        placePieces(name, Pieces(govBases = 1))
      }
      placeBases(remaining - 1, oop)
    }

    placeBases(fromOutOfPlay, true)
    placeBases(fromInPlay, false)
  }
  override def executeShaded(role: Role): Unit = {
    // Siphoned: FLN +3 resources
    increaseResources(Fln, 3)
  }
}

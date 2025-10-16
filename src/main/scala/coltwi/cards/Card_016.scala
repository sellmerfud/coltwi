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

object Card_016 extends Card(16, "NATO", Dual, false, false)
{
  override def botEventSelection(): EventSelection = NoEvent  // Bot never plays this event
  override def executeUnshaded(role: Role): Unit = {
    // Force de Frappe releases conventional troops:
    // Move 1d6 French cubes from Out of Play to Available.
    if (game.outOfPlay.frenchCubes == 0)
      log("No French cubes are out of play. The event has no effect")
    else {
      val die = dieRoll
      log(s"Die roll is: $die")
      val cubes = if (game.outOfPlay.frenchCubes <= die)
        game.outOfPlay.only(FRENCH_CUBES)
      else if (game.outOfPlay.frenchTroops == 0)
        Pieces(frenchPolice = die)
      else if (game.outOfPlay.frenchPolice == 0)
        Pieces(frenchTroops = die)
      else
        askPieces(game.outOfPlay, die, FRENCH_CUBES, Some("Move cubes from Out of Play to Available"))

      movePiecesFromOutOfPlayToAvailable(cubes)
    }
  }
  // Continental war scare: Move 1d6 French cubes from available or
  // the map to Out of Play (Government player's choice)
  override def executeShaded(role: Role): Unit = ()
}

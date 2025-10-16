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

object Card_001 extends Card(1, "Quadrillage", Dual, false, false)
{
  override def botEventSelection(): EventSelection = NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Sector forces: Place up to all French Police in Available
    // in up to 3 spaces.
    def nextSpace(remaining: Int, candidates: List[String]): Unit = {
      if (remaining > 0 && game.frenchPoliceAvailable > 0) {
        val choices = List(
          "space" -> "Select a space to place French Police",
          "done"  -> "Finished")
        println(s"\nFrench police in the available box: ${game.frenchPoliceAvailable}")
        askMenu(choices).head match {
          case "space" =>
            val name = askSpace("Which space: ", candidates)
            val num  = askInt("How many French Police", 0, game.frenchPoliceAvailable)
            placePieces(name, Pieces(frenchPolice = num))
            nextSpace(remaining - 1, candidates filterNot (_ == name))
          case _ =>
        }
      }
    }
    if (game.frenchPoliceAvailable > 0)
      nextSpace(3, spaceNames(game.algerianSpaces).sorted)
    else
      log("There are no French Police in the available box")
  }
  // Bot never executes this event
  override def executeShaded(role: Role): Unit = ()
}

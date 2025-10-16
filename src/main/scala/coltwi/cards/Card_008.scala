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

object Card_008 extends Card(8, "Cross-border air strike", Dual, false, false)
{
  override def botEventSelection(): EventSelection = NoEvent  // Bot never plays this event
  override def executeUnshaded(role: Role): Unit = {
    // Effective: Remove up to 3 Guerrillas (may be underground) from
    // either Morocco or Tunisia to available.
    if (game.moroccoTunisiaIndependent) {
      val morocco = game.getSpace(Morocco)
      val tunisia = game.getSpace(Tunisia)
      val country = if (morocco.totalGuerrillas == 0 && tunisia.totalGuerrillas == 0) "none"
      else if (morocco.totalGuerrillas == 0) Tunisia
      else if (tunisia.totalGuerrillas == 0) Morocco
      else {
        println("\nRemove guerrillas from which country:")
        askMenu(List(Morocco -> Morocco, Tunisia -> Tunisia)).head
      }

      if (country == "none")
        log("No guerrillas in Morocco or Tunisia. The event has no effect")
      else {
        val sp = game.getSpace(country)
        val guerrillas = askPieces(sp.pieces, 3 min sp.totalGuerrillas, GUERRILLAS)
        removeToAvailable(country, guerrillas)
      }
    }
    else
      log("Morocco and Tunisia are not independent. The event has no effect")
  }
  override def executeShaded(role: Role): Unit = ()
}

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

object Card_049 extends Card(49, "Factional Plot", Dual, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection = {
    val morocco = game.getSpace(Morocco)
    val tunisia = game.getSpace(Tunisia)
    val maxAllowed = (morocco.totalGuerrillas + tunisia.totalGuerrillas + 1) / 2
    if (game.moroccoTunisiaIndependent &&
          (morocco.totalGuerrillas > maxAllowed || tunisia.totalGuerrillas > maxAllowed))
      Shaded
    else
      NoEvent
  }
  override def executeUnshaded(role: Role): Unit = {
    // Plot successful, mass confusion: Remove half of Guerrillas in
    // Tunisia to Available (round down)
    val sp = game.getSpace(Tunisia)
    if (sp.totalGuerrillas == 0)
      log("There are no guerrillas in Tunisia")
    else {
      val num = sp.totalGuerrillas / 2
      val guerrillas = askPieces(sp.pieces, sp.totalGuerrillas / 2, GUERRILLAS)
      removeToAvailable(Tunisia, guerrillas)
    }
  }
  override def executeShaded(role: Role): Unit = {
    // Plot crushed, organizational realignment: May redistribute any number
    // of Guerrillas in Morocco and Tunisia between these two countries.
    val morocco = game.getSpace(Morocco)
    val tunisia = game.getSpace(Tunisia)
    val maxAllowed = (morocco.totalGuerrillas + tunisia.totalGuerrillas + 1) / 2
    if (morocco.totalGuerrillas > maxAllowed) {
      val num = morocco.totalGuerrillas - maxAllowed
      val numHidden = (morocco.hiddenGuerrillas - (morocco.hiddenGuerrillas + tunisia.hiddenGuerrillas + 1) / 2) max 0
      val guerrillas = Pieces(hiddenGuerrillas = numHidden, activeGuerrillas = num - numHidden)
      movePieces(guerrillas, Morocco, Tunisia)
    }
    else {
      val num = tunisia.totalGuerrillas - maxAllowed
      val numHidden = (tunisia.hiddenGuerrillas - (morocco.hiddenGuerrillas + tunisia.hiddenGuerrillas + 1) / 2) max 0
      val guerrillas = Pieces(hiddenGuerrillas = numHidden, activeGuerrillas = num - numHidden)
      movePieces(guerrillas, Tunisia, Morocco)
    }
  }
}

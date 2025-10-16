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

object Card_023 extends Card(23, "Diplomatic Leanings", Dual, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
    if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Pressure on protectorates: Remove up to 1 base in each
    // of Morocco and Tunisia. (If independent). No increase in
    // Commitment.
    if (!game.moroccoTunisiaIndependent)
      log("Morocco and Tunisia are not independent: The event has no effect")
    else if (game.getSpace(Morocco).flnBases == 0 && game.getSpace(Tunisia).flnBases == 0)
      log("No FLN bases in Morocco or Tunisia: The event has no effect")
    else {
      if (game.getSpace(Morocco).flnBases > 0)
        removeToAvailable(Morocco, Pieces(flnBases = 1))
      if (game.getSpace(Tunisia).flnBases > 0)
        removeToAvailable(Tunisia, Pieces(flnBases = 1))
    }
  }
  override def executeShaded(role: Role): Unit = {
    // Arab Bloc solidarity: +6 FLN resources
    increaseResources(Fln, 6)
  }
}

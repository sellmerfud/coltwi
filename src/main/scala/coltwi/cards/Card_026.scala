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

object Card_026 extends Card(26, "Casbah", Dual, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
    if (Bot.getGuerrillasToPlace(4, game.getSpace(Algiers)).total > 0) Shaded else NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Ratonade: Remove up to all FLN pieces in Algiers to Available.
    // +1 Commitment per base removed.  +1 FLN resource for each piece
    // removed.
    val sp = game.getSpace(Algiers)
    if (sp.totalFln == 0)
      log("There are no FLN pieces in Algiers.  The event has no effect")
    else {
      val flnPieces = sp.only(Seq(HiddenGuerrillas, ActiveGuerrillas, FlnBases))
      removeToAvailable(Algiers, flnPieces)
      increaseCommitment(sp.flnBases)
      increaseResources(Fln, sp.totalFln)
    }
  }
  override def executeShaded(role: Role): Unit = {
    // Urban uprising: Place up to 4 Guerrillas in Algiers.  If this makes
    // Algiers FLN-controlled, may Agitage up to 1 level for free.
    // (Remove 1 terror marker or if there are no terror markers
    //  shift 1 level toward Oppose)
    val toPlace = Bot.getGuerrillasToPlace(4, game.getSpace(Algiers))
    Bot.placeGuerrillas(Algiers, toPlace)

    val sp = game.getSpace(Algiers)
    if (sp.isFlnControlled && sp.terror > 0)
      removeTerror(Algiers, 1)
    else if (sp.isFlnControlled && !sp.isOppose)
      decreaseSupport(Algiers, 1)
  }
}

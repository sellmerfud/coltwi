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

object Card_048 extends Card(48, "Ultras", Dual, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
    if (game hasAlgerianSpace (_.algerianCubes > 0)) Shaded else NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Freelancers: Remove up to 1 Guerrilla in each City space to available.
    val cities = (game.spaces filter (sp => sp.isCity && sp.totalGuerrillas > 0)).sortBy(_.name)
    if (cities.isEmpty)
      log("There are no guerrillas in any cities.  The event has no effect")
    else
      for (sp <- cities)
        removeToAvailable(sp.name, askPieces(sp.pieces, 1, GUERRILLAS))
  }
  override def executeShaded(role: Role): Unit = {
    // Army supresses pied-noir hotheads: Remove 1-3 (1d6 halved round up)
    // algerian cubes to Available.
    val priorities = List(
      new Bot.BooleanPriority[Space]("FLN base",      _.flnBases > 0),
      new Bot.BooleanPriority[Space]("Support space", _.isSupport))
    val die   = dieRoll
    val total = (die + 1) / 2
    log(s"Die roll is $die. $Fln may remove $total Algerian cubes")

    def removeCubes(removed: Int, pieceType: PieceType, candidates: List[Space]): Int = {
      if (removed < total && candidates.nonEmpty) {
        val sp = Bot.topPriority(candidates, priorities)
        val num = (total - removed) min sp.pieces.numOf(pieceType)
        removeToAvailable(sp.name, Pieces().set(num, pieceType))
        removeCubes(removed + num, pieceType, candidates filterNot (_.name == sp.name))
      }
      else
        removed
    }

    val troopsRemoved = removeCubes(0, AlgerianTroops, game.algerianSpaces filter (_.algerianTroops > 0))
    removeCubes(troopsRemoved, AlgerianPolice, game.algerianSpaces filter (_.algerianPolice > 0))
  }
}

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

object Card_020 extends Card(20, "Suave qui peut", Single, FlnMarked, false)
{
  override def botEventSelection(): EventSelection = NoEvent  // Bot never plays this event
  override def executeUnshaded(role: Role): Unit = {
    // Defections and desertions: Executor of Event may removes up to 3
    // Guerrillas or Algerian Police to available, pay 1 resource each.
    if (game.resources(Gov) == 0)
      log(s"$Gov has no resources.  The event has no effect")
    else {
      val totalGuerrillas = game.totalOnMap(_.totalGuerrillas)
      val totalPolice     = game.totalOnMap(_.algerianPolice)
      val choice = if (totalGuerrillas + totalPolice == 0)
        "none"
      else if (totalGuerrillas == 0)
        "police"
      else if (totalPolice == 0)
        "guerrillas"
      else {
        println("\nChoose one:")
        askMenu(List("guerrillas" -> "Remove Guerrillas", "police" -> "Remove Algerian police")).head
      }

      def removePieces(remaining: Int, types: Seq[PieceType], desc: String): Unit = {
        val candidates = spaceCandidates(_.totalOf(types) > 0)
        if (remaining > 0 && candidates.nonEmpty) {
          val name   = askSpace(s"\nSelect space to remove $desc: ", candidates)
          val sp     = game.getSpace(name)
          val most   = remaining min sp.totalOf(types)
          val num    = askInt(s"Remove how many $desc from ${sp.nameAndZone}", 0, most)
          val pieces = askPieces(sp.pieces, num, types)
          removeToAvailable(name, pieces)
          removePieces(remaining - num, types, desc)
        }
      }

      choice match {
        case "guerrillas" =>
          val num = askInt("Remove how many total guerrillas", 1, 3 min game.resources(Gov))
          decreaseResources(Gov, num)
          removePieces(num, GUERRILLAS, "guerrillas")
        case "police" =>
          val num = askInt("Remove how many total Algerian police", 1, 3 min game.resources(Gov))
          decreaseResources(Gov, num)
          removePieces(num, Seq(AlgerianPolice), "Algerian police")
        case _ => log("There are no guerrillas or Algerian police on the map.  The event has no effect")
      }
    }
  }
  override def executeShaded(role: Role): Unit = () // Single event
}

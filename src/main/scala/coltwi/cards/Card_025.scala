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

object Card_025 extends Card(25, "Purge", Single, false, false)
{
  override def botEventSelection(): EventSelection = Unshaded
  override def executeUnshaded(role: Role): Unit = {
    // Disloyal commanders: 1-3 enemy pieces (1d6 halved, round up,
    // executor's choice, but not bases) moved from map to Available;
    // And -1d6 enemy resources
    val piecesDie = dieRoll
    val numPieces = (piecesDie + 1) / 2
    val resourcesDie = dieRoll
    if (role == Gov) {
      def removeGuerrillas(remaining: Int): Unit = {
        val candidates = spaceCandidates(_.totalGuerrillas > 0)
        if (remaining > 0 && candidates.nonEmpty) {
          val name   = askSpace(s"\nSelect space to remove guerrillas: ", candidates)
          val sp     = game.getSpace(name)
          val most   = remaining min sp.totalGuerrillas
          val num    = askInt(s"Remove how many guerrillas from ${sp.nameAndZone}", 0, most)
          val pieces = askPieces(sp.pieces, num, GUERRILLAS)
          removeToAvailable(name, pieces)
          removeGuerrillas(remaining - num)
        }
      }

      val guerrillasOnMap = game.totalOnMap(_.totalGuerrillas)
      if (guerrillasOnMap == 0)
        log("No guerrillas on the map to remove.")
      else {
        log(s"Die roll for guerrillas is $piecesDie.  $Gov may remove up to ${amountOf(numPieces, "guerrilla")}")
        removeGuerrillas(numPieces min guerrillasOnMap)
      }
      log(s"Die roll for resources is $resourcesDie")
      decreaseResources(Fln, resourcesDie)
    }
    else {
      var numRemoved = 0
      val basePriorities = List(
        new Bot.BooleanPriority[Space]("Has FLN Base",    _.flnBases > 0),
        new Bot.LowestPriority[Space]("Least guerrillas", _.totalGuerrillas))
      val otherPriorities = List(
        new Bot.BooleanPriority[Space]("Guerrilla present", _.totalGuerrillas > 0),
        new Bot.BooleanPriority[Space]("Gov control",       _.isGovControlled),
        new Bot.HighestPriority[Space]("Most guerrillas",   _.totalGuerrillas))

      log(s"Die roll for pieces is $piecesDie.  $Fln may remove up to $numPieces enemy pieces")
      def doRemove(candiates: List[Space], pieceType: PieceType): Unit = {
        if (numRemoved < numPieces && candiates.nonEmpty) {
          val sp = if (candiates exists (_.flnBases > 0))
            Bot.topPriority(candiates, basePriorities)
          else
            Bot.topPriority(candiates, otherPriorities)
          val num = (numPieces - numRemoved) min sp.pieces.numOf(pieceType)
          numRemoved += num
          removeToAvailable(sp.name, Pieces().set(num, pieceType))
          doRemove(candiates filterNot(_.name == sp.name), pieceType)
        }
      }

      // French before Algerians, then or within that, Troops before Police.
      doRemove(game.algerianSpaces filter (_.frenchTroops   > 0), FrenchTroops)
      doRemove(game.algerianSpaces filter (_.frenchPolice   > 0), FrenchPolice)
      doRemove(game.algerianSpaces filter (_.algerianTroops > 0), AlgerianTroops)
      doRemove(game.algerianSpaces filter (_.algerianPolice > 0), AlgerianPolice)

      log(s"Die roll for resources is $resourcesDie")
      decreaseResources(Gov, resourcesDie)
    }
  }
  override def executeShaded(role: Role): Unit = () // Single event
}

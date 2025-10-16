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

object Card_054 extends Card(54, "Operation 744", Single, FlnMarked, AlwaysPlay)
{
  val op744 = (sp: Space) =>
    sp.isMountains &&
    sp.population == 0 &&
    !flnBaseAdjacent(sp)

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(_.frenchTroops > 0) && game.hasAlgerianSpace(op744))
      Unshaded
    else
      NoEvent

  override def executeUnshaded(role: Role): Unit = {
    // Beat the bushes: Executor of Event moves up to 4 French troops
    // from any spaces to 1 Mountain sector.  Remove up to 2 FLN pieces
    // (may be Underground, but Guerrillas before bases) to Available
    // if any there.
    if (role == Gov) {
      val destName = askSpace("\nSelect mountain sector: ", algerianCandidates(_.isMountains))
      val dest = game.getSpace(destName)
      val withMovableTroops = (sp: Space) => sp.name != destName && sp.frenchTroops > 0
      val movableTroops = game.totalOnMap(_.frenchTroops) - dest.frenchTroops
      val mostTroops = 4 min movableTroops
      val total = askInt(s"Move how many French troops to ${dest.nameAndZone}", 0, mostTroops)
      def moveTroops(remaining: Int): Unit = if (remaining > 0) {
        val name = askSpace("\nSelect space with French troops: ", algerianCandidates(withMovableTroops))
        val sp   = game.getSpace(name)
        val num  = askInt(s"Move how many French Troops out of ${sp.nameAndZone}", 0, remaining min sp.frenchTroops)
        movePieces(Pieces(frenchTroops = num), name, destName)
        moveTroops(remaining - num)
      }

      if (total == movableTroops)
        for (sp <- game.algerianSpaces.filter(withMovableTroops))
          movePieces(sp.only(FrenchTroops), sp.name, destName)
      else
        moveTroops(total)

      val guerrillas = if (dest.totalGuerrillas <= 2)
        dest.pieces.only(GUERRILLAS)
      else
        askPieces(dest.pieces, 2, GUERRILLAS)
      val bases = Pieces(flnBases = (2 - guerrillas.total) min dest.flnBases)
      // Note: There is no change in commitment if a base is removed.
      removeToAvailable(destName, guerrillas + bases)
    }
    else { // role == Fln
      val priorities = List(
        new Bot.LowestPriority[Space]("Fewest guerrillas", _.totalGuerrillas))
      val dest = Bot.topPriority(game.algerianSpaces filter op744, priorities)
      def moveTroops(remaining: Int, sources: List[Space]): Unit = (remaining, sources) match {
        case (0, _)   =>
        case (_, Nil) =>
        case (_, x :: xs) =>
          val num = remaining min x.frenchTroops
          movePieces(Pieces(frenchTroops = num), x.name, dest.name)
          moveTroops(remaining - num, xs)
      }
      val sources = game.algerianSpaces filter (_.frenchTroops > 0) sortBy (-_.frenchTroops)
      moveTroops(4, sources)
      val num = dest.totalGuerrillas min 2
      val active = num min dest.activeGuerrillas
      val guerrillas = Pieces(activeGuerrillas = active,
                              hiddenGuerrillas = (num - active) min dest.hiddenGuerrillas)
      removeToAvailable(dest.name, guerrillas)
    }
  }
  override def executeShaded(role: Role): Unit = () // Single event
}

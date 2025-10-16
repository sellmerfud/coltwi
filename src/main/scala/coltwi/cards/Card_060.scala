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

object Card_060 extends Card(60, "Soummam Conference", Dual, FlnMarked, AlwaysPlay)
{
  val soumanConf = (sp: Space) => sp.canTakeBase && sp.flnBases == 0


  override def botEventSelection(): EventSelection =
    if (game.flnBasesAvailable > 0 && game.hasAlgerianSpace(soumanConf))
      Shaded
    else
      NoEvent
      
  override def executeUnshaded(role: Role): Unit = {
    // Inter-Wilaya wrangling: Replace up to 2 FLN Bases in any spaces
    // in Algeria with 1 guerrilla each. No change in commitment.
    def replaceBases(remaining: Int): Unit = {
      val candidates = algerianCandidates(_.flnBases > 0)
      if (remaining > 0 && candidates.nonEmpty) {
        val name = askSpace("\nSelect Algerian space with an FLN base: ", candidates)
        removeToAvailable(name, Pieces(flnBases = 1))
        if (game.guerrillasAvailable > 0)
          placePieces(name, Pieces(hiddenGuerrillas = 1))
        replaceBases(remaining - 1)
      }
    }
    if (game.hasAlgerianSpace(_.flnBases > 0))
      replaceBases(2)
    else
      log("\nThere are no FLN bases in Algeria.  The event has no effect")
  }
  override def executeShaded(role: Role): Unit = {
    // Productive meeting:  Place up to 2 FLN bases in any spaces in
    // Algeria for free
    val hiddenGuerrilla  = new Bot.BooleanPriority[Space]("Underground",   _.hiddenGuerrillas > 0)
    val atLeastTwo       = new Bot.BooleanPriority[Space]("2+ guerrillas", _.totalGuerrillas > 1)
    val noAdjacentTroops = new Bot.BooleanPriority[Space]("No adjacent troops",
      sp => !(getAdjacent(sp.name).map(game.getSpace).exists(_.totalTroops > 0))
    )
    val notResettled     = new Bot.BooleanPriority[Space]("Not resettled",   !_.isResettled)
    val mountainSpace    = new Bot.BooleanPriority[Space]("Is mountain",      _.isMountains)
    val lowestPop        = new Bot.LowestPriority[Space]("Lowest population", _.population)
    val lowestCubes      = new Bot.LowestPriority[Space]("Lowest cubes",      _.totalCubes)

    val noCubePriorities = List(hiddenGuerrilla, noAdjacentTroops, lowestPop,
                                atLeastTwo, mountainSpace, notResettled)

    val otherPriorities = List(hiddenGuerrilla, lowestCubes, atLeastTwo,
                                noAdjacentTroops, mountainSpace, lowestPop, notResettled)

    def placeBase(remaining: Int, candidates: List[Space]): Unit = {
      if (remaining > 0 && candidates.nonEmpty) {
        val sp = {
          val noCubes = candidates.filter(_.totalCubes == 0)
          if (noCubes.nonEmpty)
            Bot.topPriority(noCubes, noCubePriorities)
          else
            Bot.topPriority(candidates, otherPriorities)
        }
        placePieces(sp.name, Pieces(flnBases = 1))
        placeBase(remaining - 1, candidates.filterNot(_.name == sp.name))
      }
    }
    placeBase(2 min game.flnBasesAvailable, game.algerianSpaces.filter(soumanConf))
  }
}

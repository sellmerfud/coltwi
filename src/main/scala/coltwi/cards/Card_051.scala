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

object Card_051 extends Card(51, "Stripey Hole", Dual, FlnMarked, AlwaysPlay)
{
  val stripeyHoleUnshaded = (sp: Space) =>
    sp.isSector &&
    sp.population > 0 &&
    !sp.isOppose &&
    sp.flnBases > 0

  val stripeyHoleShaded = (sp: Space) => {
    val haveAvail = Bot.getGuerrillasToPlace(2, sp).total > 0
    sp.isSector && sp.isSupport && (haveAvail || game.outOfPlay.totalGuerrillas > 0)
  }

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(stripeyHoleShaded))
      Shaded
    else if (game.hasAlgerianSpace(stripeyHoleUnshaded))
      Unshaded
    else
      NoEvent

  // Mass arbitrary imprisonment: Activate all Guerrillas in any
  // Sector.  Set to Oppose.
  override def executeUnshaded(role: Role): Unit =
    if (role == Gov) {
      val priorities = List(
        new Bot.HighestPriority[Space]("Highest population", _.population),
        new Bot.BooleanPriority[Space]("Support Space",      _.isSupport))

      val name = if (role == Gov)
        askSpace("\nSelect sector: ", algerianCandidates(_.isSector))
      else //  role == Fln
        Bot.topPriority(game.algerianSpaces.filter(stripeyHoleUnshaded), priorities).name

      activateHiddenGuerrillas(name, game.getSpace(name).hiddenGuerrillas)
      setSupport(name, Oppose)
    }

  // Prison Break: Place 2 Guerrillas in any 1 Sector from Available or
  // Out of Play
  override def executeShaded(role: Role): Unit = {
    val priorities = List(
      new Bot.BooleanPriority[Space]("None underground",   _.hiddenGuerrillas == 0),
      new Bot.HighestPriority[Space]("Highest population", _.population))
    val sp = Bot.topPriority(game.algerianSpaces.filter(stripeyHoleShaded), priorities)
    val oop = game.outOfPlay.hiddenGuerrillas min 2
    placePiecesFromOutOfPlay(sp.name, Pieces(hiddenGuerrillas = oop))
    val toPlace = Bot.getGuerrillasToPlace(2 - oop, sp)
    Bot.placeGuerrillas(sp.name, toPlace)
  }
}

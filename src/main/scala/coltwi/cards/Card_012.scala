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

object Card_012 extends Card(12, "Ventilos", Dual, FlnMarked, AlwaysPlay)
{
  val flnNavySource = (sp: Space) =>
    sp.coastal &&
    !sp.isSupport &&
    Bot.ConsiderMarch.eligibleGuerrillas(sp, true, true).total > 0
    
  val flnNavyDest = (sp: Space) => sp.coastal && sp.isSupport


  override def botEventSelection(): EventSelection = {
    if (game.hasSpace(flnNavySource) && game.hasAlgerianSpace(flnNavyDest))
      Shaded
    else
      NoEvent
  }
  override def executeUnshaded(role: Role): Unit = {
    // S-55 helicopters: Until Propaganda round, raise number of
    // troop lift spaces by 1
    playMomentum(MoVentilos)
  }
  override def executeShaded(role: Role): Unit = {
    // FLN Navy: May redistribute 1-3 guerrillas (1d6 halved, round up)
    // among any 3 coastal spaces for free.  Guerrillas do no activate.
    val die = dieRoll
    val num = (die + 1) / 2   // Half rounded up

    val DestPriorities = List(
      new Bot.BooleanPriority[Space]("Cities",             _.isCity),
      new Bot.HighestPriority[Space]("Highest population", _.population))

    val sources = game.spaces.filter(flnNavySource)
    val dests   = game.algerianSpaces.filter(flnNavyDest)

    val guerrillas = sources
      .map(sp => (sp.name, Bot.ConsiderMarch.eligibleGuerrillas(sp, true, true)))
      .sortBy { case (_, pieces) => -pieces.total }
    val total = (guerrillas.foldLeft(0) { case (sum, (_, p)) => sum + p.total }) min num
    val (firstSrc, firstPieces) = guerrillas.head

    log(s"Die roll is $die.  $Fln may move up to $num guerrillas among 3 coastal spaces")
    if (total > 1 && firstPieces.total >= total && dests.size > 1) {
      // Move guerrillas from one space to two destinations
      val dest1 = Bot.topPriority(dests, DestPriorities)
      val dest2 = Bot.topPriority(dests filterNot (_.name == dest1.name), DestPriorities)
      movePieces(Pieces(hiddenGuerrillas = if (total == 3) 2 else 1), firstSrc, dest1.name)
      movePieces(Pieces(hiddenGuerrillas = 1), firstSrc, dest2.name)
    }
    else {
      // Move guerrillas to a single destinations
      val dest = Bot.topPriority(dests, DestPriorities)

      if (firstPieces.total >= total)
        movePieces(Pieces(hiddenGuerrillas = total), firstSrc, dest.name)
      else {
        val (secondSrc, secondPieces) = guerrillas.drop(1).head
        movePieces(firstPieces, firstSrc, dest.name)
        val from2nd = (total - firstPieces.total) min secondPieces.total
        movePieces(Pieces(hiddenGuerrillas = from2nd), secondSrc, dest.name)
      }
    }
  }
}
